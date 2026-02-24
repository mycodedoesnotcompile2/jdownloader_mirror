(function() {
  const state = { def: null, selected: null, editors: {}, nsOpen: {}, suppressHashChange: false, responsePreviewUrl: null };
  const LOG_PREFIX = '[RemoteDocs]';
  const by = (id) => document.getElementById(id);
  const esc = (v) => String(v == null ? '' : v).replace(/[&<>"']/g, (m) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[m]));
  const log = (...args) => console.log(LOG_PREFIX, ...args);
  const error = (...args) => console.error(LOG_PREFIX, ...args);

  function autoResizePre(el) {
    if (!el) return;
    el.style.height = 'auto';
    el.style.height = el.scrollHeight + 'px';
  }

  function setPre(id, text) {
    const el = by(id);
    if (!el) return;
    el.textContent = text;
    autoResizePre(el);
  }

  function clearResponsePreview() {
    if (state.responsePreviewUrl) {
      try {
        URL.revokeObjectURL(state.responsePreviewUrl);
      } catch (e) {
      }
      state.responsePreviewUrl = null;
    }
    const box = by('responsePreviewBox');
    const img = by('responsePreviewImg');
    if (img) {
      img.removeAttribute('src');
    }
    if (box) {
      box.style.display = 'none';
    }
  }

  function showImagePreview(blob, contentType) {
    const box = by('responsePreviewBox');
    const img = by('responsePreviewImg');
    if (!box || !img || !blob) return;
    clearResponsePreview();
    state.responsePreviewUrl = URL.createObjectURL(blob);
    img.src = state.responsePreviewUrl;
    img.alt = contentType || 'image response';
    box.style.display = 'block';
  }

  function formatExampleJsonText(raw) {
    const txt = String(raw == null ? '' : raw).trim();
    if (!txt) return '';
    try {
      return JSON.stringify(JSON.parse(txt), null, 2);
    } catch (e) {
      return txt;
    }
  }

  function formatRequestPreview(method, url, headers, body) {
    const lines = [];
    lines.push(method + ' ' + url + ' HTTP/1.1');
    lines.push('Host: ' + location.host);
    lines.push('Origin: ' + location.origin);
    lines.push('Referer: ' + location.href);
    Object.keys(headers || {}).forEach((k) => lines.push(k + ': ' + headers[k]));
    lines.push('');
    lines.push(body || '(empty)');
    lines.push('');
    lines.push('Note: Browser-managed headers (e.g. sec-fetch-*, connection, content-length, cookie) are not readable from JavaScript.');
    return lines.join('\n');
  }

  function urlWithToken(base) {
    const t = by('token').value.trim();
    log('urlWithToken', { base, hasToken: !!t });
    // If token exists, we send it via X-TOKEN header only (no token query parameter).
    return base;
  }

  function authHeaders() {
    const t = by('token').value.trim();
    if (!t) return {};
    return { 'X-TOKEN': t };
  }

  function typeHint(type) {
    if (!type) return '';
    const x = String(type).toLowerCase();
    if (x === 'string') return 'e.g. hello';
    if (x === 'boolean' || x === 'boolean|null') return 'true/false';
    if (/int|long|short|double|float|number|byte/.test(x)) return 'e.g. 123';
    if (x.includes('list') || x.includes('set') || x.includes('map') || x.includes('{') || x.includes('[')) return 'JSON, e.g. [] or {}';
    return 'Value or JSON';
  }

  function parseParameterValue(rawValue) {
    const raw = String(rawValue == null ? '' : rawValue).trim();
    if (raw === '') return null;
    if (raw === 'null') return null;
    if (raw === 'true') return true;
    if (raw === 'false') return false;
    if (/^-?\d+(\.\d+)?$/.test(raw)) return Number(raw);
    if ((raw.startsWith('[') && raw.endsWith(']')) || (raw.startsWith('{') && raw.endsWith('}')) || (raw.startsWith('"') && raw.endsWith('"'))) {
      try {
        return JSON.parse(raw);
      } catch (e) {
        return raw;
      }
    }
    return raw;
  }

  function isApiEnvelope(payload) {
    return !!(payload && typeof payload === 'object' && ('data' in payload) && (payload.type || payload.src));
  }

  function unwrapApiPayload(payload) {
    if (isApiEnvelope(payload)) {
      return payload.data;
    }
    return payload;
  }

  function simpleClassName(name) {
    const raw = String(name == null ? '' : name).trim();
    if (!raw) return '';
    const genericIdx = raw.indexOf('<');
    if (genericIdx > 0) return raw;
    const lastDot = raw.lastIndexOf('.');
    if (lastDot >= 0 && lastDot + 1 < raw.length) return raw.substring(lastDot + 1);
    return raw;
  }

  function normalizeJavaType(raw) {
    let s = String(raw == null ? '' : raw).trim();
    s = s.replace(/^class\s+/, '').replace(/^interface\s+/, '').trim();
    return s;
  }

  function extractCandidateJavaTypes(raw) {
    const s = normalizeJavaType(raw);
    if (!s) return [];
    const set = {};
    const out = [];
    const add = (v) => {
      const t = normalizeJavaType(v);
      if (!t || set[t]) return;
      set[t] = true;
      out.push(t);
    };
    add(s);
    const rx = /[A-Za-z_][A-Za-z0-9_]*(?:\.[A-Za-z_][A-Za-z0-9_]*)+/g;
    let m = null;
    while ((m = rx.exec(s)) != null) add(m[0]);
    return out;
  }

  function isSimpleBuiltinType(javaType) {
    const t = normalizeJavaType(javaType).toLowerCase();
    if (!t) return true;
    if (t.startsWith('java.') || t.startsWith('javax.') || t.startsWith('sun.')) return true;
    if (t === 'string' || t === 'boolean' || t === 'int' || t === 'long' || t === 'short' || t === 'double' || t === 'float' || t === 'byte' || t === 'char') return true;
    return false;
  }

  function renderTags(tags) {
    const list = Array.isArray(tags) ? tags : [];
    if (!list.length) return '';
    let html = '<div class="tags">';
    list.forEach((tag) => {
      const name = String((tag && tag.name) || '').trim();
      if (!name) return;
      const desc = String((tag && tag.description) || '').trim();
      const icon = String((tag && tag.icon) || '').trim();
      const lower = name.toLowerCase();
      const clazz = lower === 'experimental' ? ' exp' : (lower === 'deprecated' ? ' dep' : '');
      const title = desc ? ' title="' + esc(desc) + '"' : '';
      html += '<span class="tag' + clazz + '"' + title + '>' + (icon ? '<span class="tag-icon">' + esc(icon) + '</span>' : '') + esc(name) + '</span>';
    });
    html += '</div>';
    return html;
  }

  function renderWikiLinks(links) {
    const list = Array.isArray(links) ? links.filter((x) => !!x) : [];
    if (!list.length) return '';
    let html = '<div class="muted" style="margin-top:6px">Wiki: ';
    list.forEach((url, idx) => {
      if (idx > 0) html += ' | ';
      html += '<a href="' + esc(url) + '" target="_blank" rel="noopener noreferrer">' + esc(url) + '</a>';
    });
    html += '</div>';
    return html;
  }

  function normalizeWikiRefToPath(wikiRef) {
    const raw = String(wikiRef == null ? '' : wikiRef).trim();
    if (!raw) return '';
    if (raw.startsWith('http://') || raw.startsWith('https://')) return raw;
    const colon = raw.indexOf(':');
    const idPart = (colon >= 0 ? raw.substring(colon + 1) : raw).trim();
    if (!idPart) return '';
    const normalizedIdPart = idPart.replace(/%23/gi, '#');
    const hash = normalizedIdPart.indexOf('#');
    const page = (hash >= 0 ? normalizedIdPart.substring(0, hash) : normalizedIdPart).trim().replace(/ /g, '_');
    if (!page) return '';
    const anchor = hash >= 0 ? normalizedIdPart.substring(hash).trim().replace(/ /g, '_') : '';
    return page + anchor;
  }

  function resolveWikiRefUrl(ref, wikiLinks) {
    const raw = String(ref == null ? '' : ref).trim();
    if (!raw) return '';
    if (raw.startsWith('http://') || raw.startsWith('https://')) return raw;
    const targetPath = normalizeWikiRefToPath(raw);
    if (!targetPath) return '';
    const links = Array.isArray(wikiLinks) ? wikiLinks.filter((x) => !!x) : [];
    const lowerTarget = targetPath.toLowerCase();
    for (const link of links) {
      const plain = String(link);
      const lowerPlain = plain.toLowerCase();
      if (lowerPlain.endsWith('/' + lowerTarget) || lowerPlain.endsWith(lowerTarget)) {
        return plain;
      }
    }
    if (links.length) {
      const sample = links[0];
      const idxHash = sample.indexOf('#');
      const noHash = idxHash >= 0 ? sample.substring(0, idxHash) : sample;
      const idxSlash = noHash.lastIndexOf('/');
      if (idxSlash > 0) {
        return noHash.substring(0, idxSlash + 1) + targetPath;
      }
    }
    return targetPath;
  }

  function renderDocText(text, wikiLinks) {
    const input = String(text == null ? '' : text);
    if (!input) return '';
    const re = /\[\[([^\]]+)\]\]/g;
    let last = 0;
    let out = '';
    let match = null;
    while ((match = re.exec(input)) != null) {
      out += esc(input.substring(last, match.index));
      const wikiRef = String(match[1] || '').trim();
      const href = resolveWikiRefUrl(wikiRef, wikiLinks);
      if (href) {
        out += '<a href="' + esc(href) + '" target="_blank" rel="noopener noreferrer">' + esc(wikiRef) + '</a>';
      } else {
        out += esc(match[0]);
      }
      last = match.index + match[0].length;
    }
    out += esc(input.substring(last));
    return out.replace(/\n/g, '<br>');
  }

  function renderLifecycleInfo(doc) {
    if (!doc) return '';
    const availableSince = String(doc.availableSince || '').trim();
    const availableMsg = String(doc.availableSinceMessage || '').trim();
    const deprecatedSince = String(doc.deprecatedSince || '').trim();
    const deprecatedMsg = String(doc.deprecatedSinceMessage || '').trim();
    if (!availableSince && !deprecatedSince) return '';
    let html = '<div class="muted" style="margin-top:6px">';
    if (availableSince) {
      html += 'Available since: <code>' + esc(availableSince) + '</code>';
      if (availableMsg) html += ' - ' + esc(availableMsg);
    }
    if (availableSince && deprecatedSince) html += '<br>';
    if (deprecatedSince) {
      html += 'Deprecated since: <code>' + esc(deprecatedSince) + '</code>';
      if (deprecatedMsg) html += ' - ' + esc(deprecatedMsg);
    }
    html += '</div>';
    return html;
  }

  function typeLink(displayName, javaType) {
    const d = simpleClassName(displayName || javaType || '');
    const t = String(javaType || '').trim();
    if (!d) return '';
    if (!t) return '<code>' + esc(d) + '</code>';
    return '<span class="type-link" data-type="' + esc(t) + '" data-display="' + esc(d) + '">' + esc(d) + '</span>';
  }

  function renderTypeRefs(displayType, javaType) {
    const label = String(displayType || simpleClassName(javaType || '') || '').trim();
    if (!label) return '<code></code>';
    const target = firstBrowsableType(javaType || displayType);
    if (!target) {
      return '<code>' + esc(label) + '</code>';
    }
    return typeLink(label, target);
  }

  function renderReturnTypes(ep) {
    const list = Array.isArray(ep && ep.returnTypes) ? ep.returnTypes : [];
    if (list.length) {
      return list.map((rt) => typeLink(rt && rt.type, rt && rt.javaType)).join(' | ');
    }
    return typeLink(ep && ep.returnType || 'void', ep && ep.returnJavaType || '');
  }

  function exceptionTypeLink(exceptionDoc) {
    if (!exceptionDoc) return '';
    const rawType = String(exceptionDoc.type || '').trim();
    const display = simpleClassName(exceptionDoc.simpleName || rawType || '');
    if (!rawType) return '<code>' + esc(display) + '</code>';
    return '<span class="type-link" data-type="__exception__:' + esc(rawType) + '" data-display="' + esc(display) + '">' + esc(display) + '</span>';
  }

  function setHashValue(hashValue) {
    const next = String(hashValue || '');
    const normalized = next.startsWith('#') ? next : ('#' + next);
    if (location.hash === normalized) return;
    state.suppressHashChange = true;
    location.hash = normalized;
    setTimeout(() => { state.suppressHashChange = false; }, 0);
  }

  function updateHashForEndpoint(path) {
    if (!path) return;
    setHashValue('/endpoint/' + encodeURIComponent(path));
  }

  function updateHashForType(javaType) {
    if (!javaType) return;
    setHashValue('/type/' + encodeURIComponent(javaType));
  }

  function parseHashRoute() {
    const hash = String(location.hash || '').replace(/^#/, '');
    if (!hash) return null;
    const endpointPrefix = '/endpoint/';
    const typePrefix = '/type/';
    if (hash.startsWith(endpointPrefix)) {
      const path = decodeURIComponent(hash.substring(endpointPrefix.length));
      return path ? { kind: 'endpoint', value: path } : null;
    }
    if (hash.startsWith(typePrefix)) {
      const javaType = decodeURIComponent(hash.substring(typePrefix.length));
      return javaType ? { kind: 'type', value: javaType } : null;
    }
    return null;
  }

  function findTypeDoc(javaType) {
    if (!state.def || !Array.isArray(state.def.types)) return null;
    if (!javaType) return null;
    const normalized = normalizeJavaType(javaType);
    const exact = state.def.types.find((t) => t && normalizeJavaType(t.javaType) === normalized);
    if (exact) return exact;
    const byName = state.def.types.find((t) => t && String(t.name || '') === String(simpleClassName(normalized)));
    if (byName) return byName;
    const candidates = extractCandidateJavaTypes(normalized);
    for (const c of candidates) {
      const match = state.def.types.find((t) => t && normalizeJavaType(t.javaType) === c);
      if (match) return match;
    }
    return null;
  }

  function findExceptionDocByType(javaType) {
    const normalized = normalizeJavaType(javaType);
    if (!normalized || !state.def || !Array.isArray(state.def.endpoints)) return null;
    for (const ep of state.def.endpoints) {
      const exceptions = Array.isArray(ep && ep.exceptions) ? ep.exceptions : [];
      for (const ex of exceptions) {
        if (!ex) continue;
        if (normalizeJavaType(ex.type) === normalized) return ex;
      }
    }
    return null;
  }

  function firstBrowsableType(javaType) {
    const list = extractCandidateJavaTypes(javaType);
    for (const t of list) {
      if (!isSimpleBuiltinType(t) && findTypeDoc(t)) return t;
    }
    return null;
  }

  function truncateText(value, max) {
    const txt = String(value == null ? '' : value).trim();
    if (!txt) return '';
    if (!max || txt.length <= max) return txt;
    return txt.substring(0, max - 1) + '…';
  }

  function renderBrowserTypeRef(displayType, javaType) {
    const label = String(displayType || simpleClassName(javaType || '') || '').trim();
    if (!label) return '<code></code>';
    const target = firstBrowsableType(javaType || displayType);
    if (!target) return '<code>' + esc(label) + '</code>';
    return '<button type="button" class="type-nav-link" data-nav-type="' + esc(target) + '" data-nav-label="' + esc(label) + '">' + esc(label) + '</button>';
  }

  function renderEnumValues(doc) {
    if (!doc) return '<div class="muted">No enum metadata available.</div>';
    const valueDocs = Array.isArray(doc.enumValueDocs) ? doc.enumValueDocs : [];
    if (valueDocs.length) {
      const rows = valueDocs.map((entry) => {
        const name = String((entry && entry.name) || '').trim();
        const desc = String((entry && entry.description) || '').trim();
        const example = String((entry && entry.example) || '').trim();
        const wiki = renderWikiLinks(entry && entry.wikiLinks);
        return '<tr><td><code>' + esc(name) + '</code></td><td>' + (desc ? esc(desc) : '<span class="muted">-</span>') + '</td><td>' + (wiki || '<span class="muted">-</span>') + '</td><td>' + (example ? '<pre>' + esc(formatExampleJsonText(example)) + '</pre>' : '<span class="muted">-</span>') + '</td></tr>';
      }).join('');
      return '<h3>Enum values</h3><table><tr><th>Value</th><th>Description</th><th>Wiki</th><th>Example</th></tr>' + rows + '</table>';
    }
    const values = Array.isArray(doc.enumValues) ? doc.enumValues : [];
    if (!values.length) return '<div class="muted">No enum values documented.</div>';
    return '<h3>Enum values</h3><pre>' + esc(JSON.stringify(values, null, 2)) + '</pre>';
  }

  function renderEnumValuesExplorer(doc) {
    if (!doc) return '<div class="muted">No enum metadata available.</div>';
    const valueDocs = Array.isArray(doc.enumValueDocs) ? doc.enumValueDocs : [];
    if (valueDocs.length) {
      const rows = valueDocs.map((entry) => {
        const name = String((entry && entry.name) || '').trim();
        const desc = String((entry && entry.description) || '').trim();
        return '<tr><td><code>' + esc(name) + '</code></td><td>' + (desc ? esc(desc) : '<span class="muted">-</span>') + '</td></tr>';
      }).join('');
      return '<h3>Enum values</h3><table><tr><th>Value</th><th>Description</th></tr>' + rows + '</table>';
    }
    const values = Array.isArray(doc.enumValues) ? doc.enumValues : [];
    if (!values.length) return '<div class="muted">No enum values documented.</div>';
    return '<h3>Enum values</h3><pre>' + esc(JSON.stringify(values, null, 2)) + '</pre>';
  }

  function renderNestedTypeSummary(javaType) {
    const nestedType = firstBrowsableType(javaType);
    if (!nestedType) return '<div class="muted">No nested object metadata.</div>';
    const nestedDoc = findTypeDoc(nestedType);
    if (!nestedDoc) {
      return '<div class="muted">Nested type metadata is not in the current definition.</div>';
    }
    if (String(nestedDoc.kind || '').toLowerCase() === 'enum') {
      return '<div class="muted" style="margin-bottom:6px">Nested type: <code>' + esc(simpleClassName(nestedType)) + '</code></div>' + renderEnumValuesExplorer(nestedDoc);
    }
    const fields = Array.isArray(nestedDoc.fields) ? nestedDoc.fields : [];
    if (!fields.length) {
      return '<div class="muted">Nested type has no fields.</div>';
    }
    const rows = fields.slice(0, 24).map((f) =>
      '<tr><td>' + esc(f.name || '') + '</td><td>' + renderBrowserTypeRef(f.type || f.javaType || '', f.javaType || f.type) + '</td><td>' + esc(truncateText(f.description || '', 120)) + '</td></tr>'
    ).join('');
    const more = fields.length > 24 ? '<div class="muted" style="margin-top:6px">+' + (fields.length - 24) + ' more fields.</div>' : '';
    return '<div class="muted" style="margin-bottom:6px">Nested type: <code>' + esc(simpleClassName(nestedType)) + '</code></div>' +
      '<table><tr><th>Field</th><th>Type</th><th>Description</th></tr>' + rows + '</table>' + more;
  }

  function renderFieldDetail(field) {
    if (!field) return '<div class="muted">Select a field from the left side.</div>';
    return '<h3 style="margin-top:0">Field: ' + esc(field.name || '') + '</h3>' +
      '<div class="muted">Type: ' + renderBrowserTypeRef(field.type || field.javaType || '', field.javaType || field.type || '') + '</div>' +
      (field.description ? '<div style="margin-top:8px;white-space:normal">' + renderDocText(field.description, field.wikiLinks) + '</div>' : '<div class="muted" style="margin-top:8px">No description.</div>') +
      renderLifecycleInfo(field) +
      renderWikiLinks(field.wikiLinks) +
      (field.example ? '<h3>Example</h3><pre>' + esc(formatExampleJsonText(field.example)) + '</pre>' : '') +
      '<h3>Nested Structure</h3>' + renderNestedTypeSummary(field.javaType || field.type);
  }

  function setupTypeBrowser(initialDoc) {
    const navEl = by('typeBrowserNav');
    const listEl = by('typeFieldList');
    const detailEl = by('typeFieldDetail');
    const filterEl = by('typeFieldFilter');
    if (!navEl || !listEl || !detailEl) return;
    const stack = [];
    if (initialDoc) stack.push(initialDoc);
    const selectedByType = {};

    const currentDoc = () => (stack.length ? stack[stack.length - 1] : null);

    const renderBrowserNav = () => {
      const labels = stack.map((d, idx) => {
        const active = idx === stack.length - 1 ? ' active' : '';
        return '<button type="button" class="type-crumb' + active + '" data-crumb-index="' + idx + '">' + esc(simpleClassName((d && (d.name || d.javaType)) || 'Type')) + '</button>';
      }).join('<span class="type-crumb-sep">/</span>');
      navEl.innerHTML =
        '<button type="button" id="typeBrowserBack" class="type-browser-back"' + (stack.length <= 1 ? ' disabled' : '') + '>Back</button>' +
        '<div class="type-crumbs">' + labels + '</div>';
      const backBtn = by('typeBrowserBack');
      if (backBtn) {
        backBtn.onclick = () => {
          if (stack.length > 1) {
            stack.pop();
            renderBrowser();
          }
        };
      }
      navEl.querySelectorAll('[data-crumb-index]').forEach((node) => {
        node.addEventListener('click', () => {
          const idx = Number(node.getAttribute('data-crumb-index'));
          if (!Number.isNaN(idx) && idx >= 0 && idx < stack.length) {
            stack.splice(idx + 1);
            renderBrowser();
          }
        });
      });
    };

    const navigateToType = (rawType) => {
      const targetType = firstBrowsableType(rawType) || normalizeJavaType(rawType);
      if (!targetType) return;
      const doc = findTypeDoc(targetType);
      if (!doc) {
        openTypePanel(targetType, simpleClassName(targetType));
        return;
      }
      const top = currentDoc();
      if (top && normalizeJavaType(top.javaType) === normalizeJavaType(doc.javaType)) return;
      stack.push(doc);
      if (filterEl) filterEl.value = '';
      renderBrowser();
    };

    const renderBrowser = () => {
      const doc = currentDoc();
      if (!doc) {
        navEl.innerHTML = '';
        listEl.innerHTML = '<div class="muted">No fields available.</div>';
        detailEl.innerHTML = '<div class="muted">No field details available.</div>';
        return;
      }
      renderBrowserNav();
      const fields = Array.isArray(doc.fields) ? doc.fields.slice() : [];
      fields.sort((a, b) => String(a && a.name || '').localeCompare(String(b && b.name || '')));
      if (!fields.length) {
        if (String(doc.kind || '').toLowerCase() === 'enum') {
          listEl.innerHTML = '<div class="muted">Enum type has no fields.</div>';
          detailEl.innerHTML = renderEnumValuesExplorer(doc);
        } else {
          listEl.innerHTML = '<div class="muted">No fields available.</div>';
          detailEl.innerHTML = '<div class="muted">No field details available.</div>';
        }
        return;
      }
      const typeKey = normalizeJavaType(doc.javaType || doc.name || '');
      let selectedIndex = selectedByType[typeKey] || 0;
      const filter = String(filterEl && filterEl.value || '').toLowerCase().trim();
      const visible = [];
      fields.forEach((f, idx) => {
        const haystack = (String(f.name || '') + ' ' + String(f.type || '') + ' ' + String(f.description || '')).toLowerCase();
        if (!filter || haystack.includes(filter)) visible.push({ f, idx });
      });
      if (!visible.length) {
        listEl.innerHTML = '<div class="muted">No fields match the filter.</div>';
        detailEl.innerHTML = '<div class="muted">No field selected.</div>';
        return;
      }
      if (!visible.some((x) => x.idx === selectedIndex)) selectedIndex = visible[0].idx;
      selectedByType[typeKey] = selectedIndex;
      listEl.innerHTML = visible.map((x) => {
        const active = x.idx === selectedIndex ? ' active' : '';
        const desc = truncateText(x.f.description || '', 84);
        return '<div class="type-field-item' + active + '">' +
          '<button type="button" class="type-field-main" data-field-index="' + x.idx + '">' +
          '<span class="type-field-item-name">' + esc(x.f.name || '') + '</span>' +
          (desc ? '<span class="type-field-item-desc">' + esc(desc) + '</span>' : '') +
          '</button>' +
          '<div class="type-field-item-type">' + renderBrowserTypeRef(x.f.type || x.f.javaType || '', x.f.javaType || x.f.type) + '</div>' +
          '</div>';
      }).join('');
      detailEl.innerHTML = renderFieldDetail(fields[selectedIndex]);
      listEl.querySelectorAll('[data-field-index]').forEach((node) => {
        node.addEventListener('click', () => {
          selectedIndex = Number(node.getAttribute('data-field-index'));
          selectedByType[typeKey] = selectedIndex;
          renderBrowser();
        });
      });
      const navNodes = listEl.querySelectorAll('.type-nav-link');
      navNodes.forEach((node) => {
        node.addEventListener('click', (ev) => {
          ev.stopPropagation();
          ev.preventDefault();
          navigateToType(node.getAttribute('data-nav-type'));
        });
      });
      detailEl.querySelectorAll('.type-nav-link').forEach((node) => {
        node.addEventListener('click', (ev) => {
          ev.preventDefault();
          navigateToType(node.getAttribute('data-nav-type'));
        });
      });
    };
    if (filterEl) {
      filterEl.oninput = renderBrowser;
    }
    renderBrowser();
  }

  function renderTypePanel(doc, title) {
    const overlay = by('typeOverlay');
    const panel = by('typePanel');
    const root = by('typeRoot');
    if (!root) return;
    if (overlay) overlay.style.display = 'flex';
    if (panel) {
      panel.style.display = 'block';
      panel.style.background = '#fff';
      panel.style.marginLeft = '0';
    }
    if (!doc) {
      root.innerHTML = '<h3>' + esc(title || 'Type details') + '</h3><div class="muted">No type metadata available.</div>';
      return;
    }
    let enums = '';
    if (String(doc.kind || '').toLowerCase() === 'enum' || (Array.isArray(doc.enumValues) && doc.enumValues.length)) {
      enums = renderEnumValues(doc);
    }
    root.innerHTML =
      '<h3>' + esc(simpleClassName(title || doc.name || doc.javaType || 'Type details')) + '</h3>' +
      '<div class="muted"><code>' + esc(simpleClassName(doc.javaType || '')) + '</code></div>' +
      (doc.description ? '<div style="margin-top:6px;white-space:normal">' + renderDocText(doc.description, doc.wikiLinks) + '</div>' : '') +
      renderLifecycleInfo(doc) +
      renderWikiLinks(doc.wikiLinks) +
      (doc.example ? '<h3>Example JSON</h3><pre>' + esc(formatExampleJsonText(doc.example)) + '</pre>' : '') +
      enums +
      '<h3>Structure Browser</h3>' +
      '<div id="typeBrowserNav" class="type-browser-nav"></div>' +
      '<div class="type-browser">' +
      '<div class="type-browser-left">' +
      '<input id="typeFieldFilter" placeholder="Filter fields">' +
      '<div id="typeFieldList" class="type-field-list"></div>' +
      '</div>' +
      '<div id="typeFieldDetail" class="type-field-detail"></div>' +
      '</div>';
    setupTypeBrowser(doc);
    attachTypeLinks(root);
  }

  function showTypeLoading(title) {
    const overlay = by('typeOverlay');
    const root = by('typeRoot');
    if (!overlay || !root) return;
    overlay.style.display = 'flex';
    root.innerHTML = '<h3>' + esc(title || 'Type details') + '</h3><div class="muted">Loading type metadata...</div>';
  }

  function showTypeError(title, msg) {
    const overlay = by('typeOverlay');
    const root = by('typeRoot');
    if (!overlay || !root) return;
    overlay.style.display = 'flex';
    root.innerHTML =
      '<h3>' + esc(title || 'Type details') + '</h3>' +
      '<div class="muted">Could not load type metadata.</div>' +
      '<pre>' + esc(String(msg || 'Unknown error')) + '</pre>';
  }

  function renderExceptionEnvelopePanel(exceptionDoc, display) {
    const root = by('typeRoot');
    if (!root) return;
    const exType = String((exceptionDoc && exceptionDoc.type) || '').trim();
    const exceptionSimple = simpleClassName(display || (exceptionDoc && exceptionDoc.simpleName) || exType || 'Exception');
    const dataJavaType = String((exceptionDoc && exceptionDoc.dataJavaType) || '').trim();
    const dataType = String((exceptionDoc && exceptionDoc.dataType) || '').trim();
    const dataTypeUi = dataJavaType ? typeLink(simpleClassName(dataType || dataJavaType), dataJavaType) : '<code>Object</code>';
    const payloadTypeLabel = simpleClassName(dataType || dataJavaType || 'Object');
    const envelopeExample = {
      src: 'DEVICE',
      type: exceptionSimple,
      data: { _type: payloadTypeLabel }
    };
    root.innerHTML =
      '<h3>' + esc(exceptionSimple) + '</h3>' +
      '<div class="muted">Exception Wire Format</div>' +
      ((exceptionDoc && exceptionDoc.description) ? '<div style="margin-top:6px;white-space:normal">' + renderDocText(exceptionDoc.description, exceptionDoc.wikiLinks) + '</div>' : '') +
      renderWikiLinks(exceptionDoc && exceptionDoc.wikiLinks) +
      '<div class="muted" style="margin-top:6px">Exceptions are returned as an envelope object with <code>src</code>, <code>type</code>, and <code>data</code>.</div>' +
      '<h3>Example JSON</h3><pre>' + esc(JSON.stringify(envelopeExample, null, 2)) + '</pre>' +
      '<h3>Fields</h3>' +
      '<table><tr><th>Name</th><th>Type</th><th>Description</th></tr>' +
      '<tr><td>src</td><td><code>String</code></td><td>Exception source (for Connect usually DEVICE).</td></tr>' +
      '<tr><td>type</td><td><code>String</code></td><td>Exception type identifier.</td></tr>' +
      '<tr><td>data</td><td>' + dataTypeUi + '</td><td>Exception-specific payload object.</td></tr>' +
      '</table>';
    attachTypeLinks(root);
  }

  async function openTypePanel(javaType, display, fromHash) {
    if (!javaType) return;
    const rawType = String(javaType || '');
    if (rawType.startsWith('__exception__:')) {
      const exceptionType = rawType.substring('__exception__:'.length);
      const exceptionDoc = findExceptionDocByType(exceptionType) || { type: exceptionType, simpleName: simpleClassName(exceptionType) };
      if (!fromHash) {
        updateHashForType(rawType);
      }
      showTypeLoading(display || exceptionType);
      renderExceptionEnvelopePanel(exceptionDoc, display || exceptionType);
      return;
    }
    const matchedException = findExceptionDocByType(rawType);
    if (matchedException) {
      if (!fromHash) {
        updateHashForType('__exception__:' + matchedException.type);
      }
      showTypeLoading(display || matchedException.simpleName || matchedException.type);
      renderExceptionEnvelopePanel(matchedException, display || matchedException.simpleName || matchedException.type);
      return;
    }
    const title = display || javaType;
    if (!fromHash) {
      updateHashForType(javaType);
    }
    showTypeLoading(title);
    let doc = findTypeDoc(javaType);
    if (!doc) {
      try {
        const url = './getStorableDefinition';
        const headers = Object.assign({ 'Content-Type': 'application/json;charset=UTF-8' }, authHeaders());
        const r = await fetch(url, { method: 'POST', headers, body: JSON.stringify([javaType]) });
        const txt = await r.text();
        if (!r.ok) throw new Error('HTTP ' + r.status + ' ' + r.statusText + ' body: ' + txt.slice(0, 250));
        const parsed = JSON.parse(txt);
        if (isApiEnvelope(parsed) && !Array.isArray(parsed.types) && !Array.isArray((parsed.data || {}).types)) {
          throw new Error('API exception: ' + String(parsed.type || 'Unknown') + '\n' + JSON.stringify(parsed.data || {}, null, 2));
        }
        const normalized = unwrapApiPayload(parsed);
        const types = normalized && Array.isArray(normalized.types) ? normalized.types : [];
        doc = types.length ? types[0] : null;
      } catch (e) {
        error('openTypePanel failed', { javaType, error: String(e) });
        showTypeError(title, e);
        return;
      }
    }
    renderTypePanel(doc, title);
  }

  function attachTypeLinks(container) {
    const root = container || by('root');
    if (!root) return;
    const nodes = root.querySelectorAll('.type-link');
    nodes.forEach((node) => {
      node.addEventListener('click', () => openTypePanel(node.getAttribute('data-type'), node.getAttribute('data-display')));
    });
  }

  function isMultiParameter(p) {
    if (!p) return false;
    if (p.multi) return true;
    const t = String(p.type || '').toLowerCase();
    const j = String(p.javaType || '').toLowerCase();
    return t.includes('list<') || t.includes('set<') || t.endsWith('[]') || j.endsWith('[]') || j.includes('list') || j.includes('set') || j.includes('collection');
  }

  function looksJsonParameter(p) {
    if (!p) return false;
    if (String(p.editor || '').toLowerCase() === 'json') return true;
    const t = String(p.type || '').toLowerCase();
    const j = String(p.javaType || '').toLowerCase();
    return t.includes('map<') || t.includes('{') || t.includes('list<') || t.includes('set<') || t.endsWith('[]') || j.includes('map');
  }

  function resolveOptionSource(source, ep) {
    if (!source) return '';
    return String(source).replace(/\{([a-zA-Z0-9_]+)\}/g, (_, name) => {
      if (!ep || !ep.parameters) return '';
      const idx = ep.parameters.findIndex((p) => p && p.name === name);
      if (idx < 0) return '';
      const value = getParameterValue(idx, ep.parameters[idx]);
      if (value == null) return '';
      if (Array.isArray(value)) return value.map((v) => String(v)).join(',');
      if (typeof value === 'object') return JSON.stringify(value);
      return String(value);
    });
  }

  function normalizeOptionValues(data) {
    const pick = Array.isArray(data) ? data : (data && Array.isArray(data.data) ? data.data : null);
    if (!pick) return [];
    const values = [];
    pick.forEach((entry) => {
      if (entry == null) return;
      if (typeof entry === 'string' || typeof entry === 'number' || typeof entry === 'boolean') {
        values.push(String(entry));
        return;
      }
      if (typeof entry === 'object') {
        if (entry.id != null) {
          values.push(String(entry.id));
          return;
        }
        if (entry.value != null) {
          values.push(String(entry.value));
          return;
        }
      }
      values.push(JSON.stringify(entry));
    });
    return Array.from(new Set(values));
  }

  function extractOptionDependencies(source) {
    const ret = [];
    if (!source) return ret;
    const seen = {};
    const re = /\{([a-zA-Z0-9_]+)\}/g;
    let match = null;
    while ((match = re.exec(String(source))) != null) {
      const key = String(match[1] || '').trim();
      if (!key || seen[key]) continue;
      seen[key] = true;
      ret.push(key);
    }
    return ret;
  }

  function refreshDynamicOptions(binding, ep, fallbackOptions) {
    if (!binding || !binding.el || !binding.param) return;
    const template = String(binding.param.optionsEndpoint || '').trim();
    if (!template) return;
    const source = resolveOptionSource(template, ep);
    if (!source) {
      setSelectOptions(binding.el, fallbackOptions || []);
      return;
    }
    const sourceUrl = source.startsWith('/') ? source : '/' + source;
    const headers = Object.assign({ 'Content-Type': 'application/json;charset=UTF-8' }, authHeaders());
    const url = urlWithToken(sourceUrl);
    const requestId = (binding.optionsRequestId || 0) + 1;
    binding.optionsRequestId = requestId;
    log('load param options', { param: binding.param.name, source, url, requestId, reason: binding.reloadReason || 'initial' });
    fetch(url, { method: 'POST', headers, body: '[]' })
      .then(async (r) => {
        const txt = await r.text();
        if (!r.ok) {
          throw new Error('HTTP ' + r.status + ' ' + r.statusText + ' body: ' + txt.slice(0, 300));
        }
        try {
          return JSON.parse(txt);
        } catch (e) {
          throw new Error('Invalid option JSON from ' + source + ': ' + txt.slice(0, 200));
        }
      })
      .then((payload) => {
        if (binding.optionsRequestId !== requestId) return;
        const dynamicOptions = normalizeOptionValues(payload);
        if (dynamicOptions.length > 0) {
          setSelectOptions(binding.el, dynamicOptions);
        } else {
          setSelectOptions(binding.el, fallbackOptions || []);
        }
      })
      .catch((e) => {
        if (binding.optionsRequestId !== requestId) return;
        error('load param options failed', { param: binding.param.name, source, error: String(e) });
      });
  }

  function wireDependentOptionReloads(ep) {
    (ep.parameters || []).forEach((param, idx) => {
      const binding = state.editors[idx];
      if (!binding || !binding.refreshOptions) return;
      const deps = Array.isArray(binding.optionDependencies) ? binding.optionDependencies : [];
      deps.forEach((depName) => {
        const depIndex = (ep.parameters || []).findIndex((p) => p && p.name === depName);
        if (depIndex < 0) return;
        const depBinding = state.editors[depIndex];
        if (!depBinding || !depBinding.el) return;
        const scheduleReload = () => {
          if (binding.reloadTimer) {
            clearTimeout(binding.reloadTimer);
          }
          binding.reloadTimer = setTimeout(() => {
            binding.reloadReason = 'dependency:' + depName;
            binding.refreshOptions();
          }, 160);
        };
        depBinding.el.addEventListener('input', scheduleReload);
        depBinding.el.addEventListener('change', scheduleReload);
      });
    });
  }

  function setSelectOptions(select, options) {
    if (!select) return;
    const previous = Array.from(select.selectedOptions || []).map((x) => x.value);
    select.innerHTML = '';
    options.forEach((value) => {
      const opt = document.createElement('option');
      opt.value = String(value);
      opt.textContent = String(value);
      if (previous.includes(opt.value)) opt.selected = true;
      select.appendChild(opt);
    });
  }

  function createParameterEditor(p, i, ep) {
    const container = by('pc' + i);
    if (!container) return;
    container.innerHTML = '';

    const editor = String(p.editor || '').toLowerCase();
    const multi = isMultiParameter(p);
    const options = Array.isArray(p.options) ? p.options : [];
    const hasSource = !!(p.optionsEndpoint && String(p.optionsEndpoint).trim());

    const binding = { kind: 'text', el: null, index: i, param: p };

    if (editor === 'checkbox') {
      const input = document.createElement('input');
      input.type = 'checkbox';
      input.checked = false;
      binding.kind = 'checkbox';
      binding.el = input;
      container.appendChild(input);
      state.editors[i] = binding;
      return;
    }

    if (editor === 'select' || editor === 'multiselect' || options.length > 0 || hasSource) {
      const select = document.createElement('select');
      if (multi || editor === 'multiselect') {
        select.multiple = true;
        binding.kind = 'select-multi';
      } else {
        binding.kind = 'select';
      }
      binding.el = select;
      container.appendChild(select);
      if (options.length > 0) {
        setSelectOptions(select, options);
      }
      if (hasSource) {
        binding.optionDependencies = extractOptionDependencies(p.optionsEndpoint);
        binding.refreshOptions = () => refreshDynamicOptions(binding, ep, options);
        binding.refreshOptions();
      }
      state.editors[i] = binding;
      return;
    }

    if (editor === 'json' || looksJsonParameter(p)) {
      const textarea = document.createElement('textarea');
      textarea.placeholder = typeHint(p.type);
      binding.kind = 'textarea';
      binding.el = textarea;
      container.appendChild(textarea);
      state.editors[i] = binding;
      return;
    }

    const input = document.createElement('input');
    input.placeholder = typeHint(p.type);
    binding.kind = 'text';
    binding.el = input;
    container.appendChild(input);
    state.editors[i] = binding;
  }

  function getParameterValue(index, p) {
    const binding = state.editors[index];
    if (!binding || !binding.el) return null;
    if (binding.kind === 'checkbox') {
      return !!binding.el.checked;
    }
    if (binding.kind === 'select') {
      const v = binding.el.value;
      return v === '' ? null : parseParameterValue(v);
    }
    if (binding.kind === 'select-multi') {
      return Array.from(binding.el.selectedOptions).map((o) => parseParameterValue(o.value));
    }
    return parseParameterValue(binding.el.value);
  }

  function renderList() {
    const filter = by('search').value.toLowerCase();
    const list = by('list');
    list.innerHTML = '';
    if (!state.def || !state.def.endpoints) return;
    log('renderList', { filter, endpointCount: state.def.endpoints.length });
    const grouped = {};
    state.def.endpoints.forEach((ep) => {
      const txt = (ep.path + ' ' + (ep.description || '')).toLowerCase();
      if (filter && !txt.includes(filter)) return;
      const ns = ep.namespace || ((ep.path || '').split('/')[0] || 'misc');
      if (!grouped[ns]) grouped[ns] = [];
      grouped[ns].push(ep);
    });
    Object.keys(grouped).sort().forEach((ns) => {
      const endpoints = grouped[ns];
      if (!endpoints || !endpoints.length) return;
      if (!(ns in state.nsOpen)) state.nsOpen[ns] = true;

      const nsWrap = document.createElement('div');
      nsWrap.className = 'ns';

      const nsTitle = document.createElement('div');
      nsTitle.className = 'ns-title';
      nsTitle.innerHTML = esc(ns) + '<span class="ns-count">(' + endpoints.length + ')</span>';
      nsTitle.onclick = () => {
        state.nsOpen[ns] = !state.nsOpen[ns];
        renderList();
      };
      nsWrap.appendChild(nsTitle);

      if (state.nsOpen[ns]) {
        const nsItems = document.createElement('div');
        nsItems.className = 'ns-items';
        endpoints.sort((a, b) => String(a.path || '').localeCompare(String(b.path || ''))).forEach((ep) => {
          const div = document.createElement('div');
          div.className = 'item' + (state.selected && state.selected.path === ep.path ? ' active' : '');
          div.innerHTML = '<div><span class="path">/' + esc(ep.path) + '</span></div><div class="muted">' + esc(ep.description || '') + '</div>' + renderTags(ep.tags);
          div.onclick = () => select(ep.path);
          nsItems.appendChild(div);
        });
        nsWrap.appendChild(nsItems);
      }

      list.appendChild(nsWrap);
    });
  }

  function select(path) {
    state.selected = (state.def.endpoints || []).find((e) => e.path === path) || null;
    if (state.selected && state.selected.path) {
      updateHashForEndpoint(state.selected.path);
    }
    renderList();
    renderMain();
  }

  function renderMain() {
    const ep = state.selected;
    const root = by('root');
    state.editors = {};
    if (!ep) {
      root.textContent = 'No endpoint selected';
      return;
    }
    log('renderMain', { path: ep.path });

    let p = '';
    (ep.parameters || []).forEach((x, i) => {
      const typeUi = typeLink(x.type || x.javaType || '', x.javaType || '');
      p += '<tr><td>' + esc(x.name || ('p' + (i + 1))) + '</td><td>' + typeUi + '</td><td id="pc' + i + '"></td></tr>';
    });

    let ex = '';
    (ep.exceptions || []).forEach((x) => {
      ex += '<tr><td>' + exceptionTypeLink(x) + '</td><td>' + esc(x.http || '') + '</td><td>' + renderDocText(x.description || '', x.wikiLinks) + '</td><td>' + renderWikiLinks(x.wikiLinks) + '</td></tr>';
    });

    let auth = '';
    (ep.auth || []).forEach((x) => {
      auth += '<div class="muted"><b>' + esc(x.name) + '</b> ' + esc(x.doc || '') + '</div>';
    });

    root.innerHTML = '<h2>' + esc(ep.path) + '</h2>' +
      '<div class="muted">' + renderDocText(ep.description || '', ep.wikiLinks) + '</div>' +
      renderLifecycleInfo(ep) +
      renderWikiLinks(ep.wikiLinks) +
      renderTags(ep.tags) +
      '<div style="height:8px"></div><div class="muted">Return: ' + renderReturnTypes(ep) + '</div>' +
      (auth ? '<div style="height:6px"></div>' + auth : '') +
      '<h3>Exceptions</h3><table><tr><th>Type</th><th>HTTP</th><th>Description</th><th>Wiki</th></tr>' +
      (ex || '<tr><td colspan="4" class="muted">No declared exceptions</td></tr>') + '</table>' +
      '<h3>Parameters</h3><table><tr><th>Name</th><th>Type</th><th>Value</th></tr>' +
      (p || '<tr><td colspan="3" class="muted">No parameters</td></tr>') +
      '</table><div style="height:10px"></div><button class="btn" id="run">Execute request</button>' +
      '<h3>Request Headers</h3><pre id="reqheaders"></pre><h3>Request Body</h3><pre id="reqbody"></pre>' +
      '<h3>Response Headers</h3><pre id="headers"></pre><h3>Response Body</h3><pre id="body"></pre>' +
      '<div id="responsePreviewBox" class="card" style="display:none;padding:10px"><h3 style="margin-top:0">Response Preview</h3><img id="responsePreviewImg" alt="response image" style="max-width:100%;height:auto;border:1px solid #d7e1e8;border-radius:8px"></div>';

    (ep.parameters || []).forEach((param, i) => createParameterEditor(param, i, ep));
    wireDependentOptionReloads(ep);
    attachTypeLinks();
    by('run').onclick = run;
  }

  function run() {
    const ep = state.selected;
    if (!ep) return;
    clearResponsePreview();
    let url = '/' + ep.path;
    const query = new URLSearchParams();
    const bodyParams = [];
    (ep.parameters || []).forEach((p, i) => {
      bodyParams.push(getParameterValue(i, p));
    });
    const qs = query.toString();
    if (qs) url += '?' + qs;
    const headers = Object.assign({ 'Content-Type': 'application/json;charset=UTF-8' }, authHeaders());
    const requestBody = JSON.stringify(bodyParams);
    setPre('reqheaders', formatRequestPreview('POST', url, headers, requestBody));
    setPre('reqbody', requestBody || '(empty)');
    log('run request', { method: 'POST', url, headers, body: requestBody });

    fetch(url, { method: 'POST', headers, body: requestBody })
      .then(async (r) => {
        log('run response', { status: r.status, statusText: r.statusText, url });
        let h = 'HTTP ' + r.status + ' ' + r.statusText + '\n';
        r.headers.forEach((v, k) => h += k + ': ' + v + '\n');
        setPre('headers', h);
        const contentType = String(r.headers.get('content-type') || '').toLowerCase();
        const isImage = contentType.startsWith('image/');
        if (isImage) {
          const blob = await r.blob();
          showImagePreview(blob, contentType);
          setPre('body', '[binary image response] ' + (contentType || '(unknown content-type)') + ', ' + blob.size + ' bytes');
        } else {
          const txt = await r.text();
          try {
            setPre('body', JSON.stringify(JSON.parse(txt), null, 2));
          } catch (e) {
            setPre('body', txt);
          }
        }
      })
      .catch((e) => {
        error('run request failed', e);
        setPre('headers', 'Error');
        setPre('body', String(e));
      });
  }

  function loadDef() {
    const url = urlWithToken('./docsDefinition');
    const headers = authHeaders();
    log('loadDef start', { url, headers });
    fetch(url, { headers })
      .then(async (r) => {
        log('loadDef response', { status: r.status, statusText: r.statusText });
        const txt = await r.text();
        if (!r.ok) {
          throw new Error('HTTP ' + r.status + ' ' + r.statusText + ' body: ' + txt.slice(0, 400));
        }
        try {
          return JSON.parse(txt);
        } catch (e) {
          throw new Error('Invalid JSON from docsDefinition: ' + txt.slice(0, 400));
        }
      })
      .then((d) => {
        const unwrapped = unwrapApiPayload(d);
        const normalized = (unwrapped && unwrapped.endpoints) ? unwrapped : d;
        log('loadDef parsed', {
          topLevelKeys: d && typeof d === 'object' ? Object.keys(d) : [],
          normalizedKeys: normalized && typeof normalized === 'object' ? Object.keys(normalized) : [],
          hasEndpoints: !!(normalized && normalized.endpoints),
          endpointCount: normalized && normalized.endpoints ? normalized.endpoints.length : 0
        });
        state.def = normalized;
        renderList();
        if (normalized.endpoints && normalized.endpoints.length) {
          const route = parseHashRoute();
          if (route && route.kind === 'endpoint') {
            const match = normalized.endpoints.find((e) => e.path === route.value);
            if (match) {
              select(match.path);
            } else {
              select(normalized.endpoints[0].path);
            }
          } else {
            select(normalized.endpoints[0].path);
          }
          if (route && route.kind === 'type' && route.value) {
            openTypePanel(route.value, simpleClassName(route.value), true);
          }
        } else {
          by('root').textContent = 'No endpoints returned by docsDefinition.';
        }
      })
      .catch((e) => {
        error('loadDef failed', e);
        by('root').textContent = 'Error while loading API definition: ' + e;
      });
  }

  window.addEventListener('error', (ev) => {
    error('window error', ev.message, ev.filename, ev.lineno, ev.colno);
  });
  window.addEventListener('unhandledrejection', (ev) => {
    error('unhandled rejection', ev.reason);
  });

  const qp = new URLSearchParams(location.search);
  if (qp.get('token')) {
    by('token').value = qp.get('token');
    log('token initialized from URL', { length: by('token').value.length });
  } else {
    let fallbackToken = null;
    try {
      const script = document.currentScript || document.querySelector('script[src*="docsJs"]');
      if (script && script.src) {
        const sUrl = new URL(script.src, location.href);
        fallbackToken = sUrl.searchParams.get('token');
      }
    } catch (e) {
      error('token fallback from script failed', e);
    }
    if (!fallbackToken) {
      try {
        const ref = document.referrer ? new URL(document.referrer) : null;
        fallbackToken = ref ? ref.searchParams.get('token') : null;
      } catch (e) {
        error('token fallback from referrer failed', e);
      }
    }
    if (fallbackToken) {
      by('token').value = fallbackToken;
      log('token initialized from fallback', { length: by('token').value.length });
    } else {
      log('no token in URL');
    }
  }
  log('docs script initialized');
  if (by('typeClose')) {
    by('typeClose').addEventListener('click', () => {
      by('typeOverlay').style.display = 'none';
      if (state.selected && state.selected.path) {
        updateHashForEndpoint(state.selected.path);
      }
    });
  }
  if (by('typeOverlay')) {
    by('typeOverlay').addEventListener('click', (ev) => {
      if (ev.target && ev.target.id === 'typeOverlay') {
        by('typeOverlay').style.display = 'none';
        if (state.selected && state.selected.path) {
          updateHashForEndpoint(state.selected.path);
        }
      }
    });
  }
  window.addEventListener('hashchange', () => {
    if (state.suppressHashChange || !state.def) return;
    const route = parseHashRoute();
    if (!route) return;
    if (route.kind === 'endpoint') {
      const match = (state.def.endpoints || []).find((e) => e.path === route.value);
      if (!match) return;
      if (state.selected && state.selected.path === match.path) return;
      select(match.path);
      return;
    }
    if (route.kind === 'type') {
      openTypePanel(route.value, simpleClassName(route.value), true);
    }
  });
  by('search').addEventListener('input', renderList);
  loadDef();
})();
