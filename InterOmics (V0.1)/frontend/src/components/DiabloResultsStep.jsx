// DiabloResultsStep.jsx — DIABLO results hub + 6 sub-pages
// Architecture: internal page state machine driven by manifest + summary.
// Pages: hub | multiblock | blockspecific | circos | network | performance | export

import { useState, useEffect, useMemo, useCallback, useRef } from 'react';
import { apiGet } from '../api/session';

// ─── Colour tokens ────────────────────────────────────────────────────────────
const C = {
  maroon:      '#7b1c2e',
  border:      '#e2e2e5',
  textPrimary: '#1a1a24',
  textMuted:   '#888896',
  textSec:     '#555563',
  bg:          '#f4f4f5',
  bgLight:     '#f9f9fb',
  white:       '#ffffff',
  green:       '#166534',
  greenBg:     '#f0fdf4',
  greenBorder: '#86efac',
  amber:       '#92400e',
  amberBg:     '#fffbeb',
  amberBorder: '#fcd34d',
};

// ─── Shared primitives ────────────────────────────────────────────────────────
function SectionTitle({ children }) {
  return (
    <div style={{ fontSize: 13, fontWeight: 700, color: C.textPrimary,
                  borderBottom: `1px solid ${C.border}`, paddingBottom: 5, marginBottom: 10 }}>
      {children}
    </div>
  );
}

function Chip({ label, value, mono = false }) {
  return (
    <div style={{ background: C.bg, border: `1px solid ${C.border}`,
                  borderRadius: 4, padding: '4px 9px', fontSize: 12 }}>
      <span style={{ color: C.textMuted }}>{label}: </span>
      <strong style={{ color: C.textPrimary, fontFamily: mono ? 'monospace' : 'inherit' }}>
        {String(value)}
      </strong>
    </div>
  );
}

function DownloadBtn({ label, url, filename }) {
  if (!url) return null;
  return (
    <a href={url} download={filename || true} style={{
      display: 'inline-flex', alignItems: 'center', gap: 4,
      fontSize: 12, color: C.maroon, fontWeight: 600,
      textDecoration: 'none', border: `1px solid ${C.maroon}`,
      borderRadius: 4, padding: '3px 9px',
    }}>
      ↓ {label}
    </a>
  );
}

// ─── PlotImg with cache-busting ───────────────────────────────────────────────
function PlotImg({ url, alt, cacheBust }) {
  const [missing, setMissing] = useState(false);
  const prevUrl = useRef(null);

  // Reset missing state when url or cacheBust changes
  useEffect(() => {
    if (url !== prevUrl.current) { setMissing(false); prevUrl.current = url; }
  }, [url, cacheBust]);

  if (!url) return <Unavailable msg="Plot not generated for this run." />;
  if (missing) return <Unavailable msg="Plot file not found." />;

  const src = cacheBust ? `${url}?t=${cacheBust}` : url;
  return (
    <img src={src} alt={alt || 'Plot'} onError={() => setMissing(true)}
      style={{ maxWidth: '100%', border: `1px solid ${C.border}`, borderRadius: 4, display: 'block' }} />
  );
}

function Unavailable({ msg }) {
  return (
    <div style={{ padding: '14px 18px', background: C.bg, border: `1px solid ${C.border}`,
                  borderRadius: 5, fontSize: 13, color: C.textMuted, fontStyle: 'italic' }}>
      {msg || 'Not available for this result.'}
    </div>
  );
}

// ─── Tab bar ──────────────────────────────────────────────────────────────────
function TabBar({ tabs, active, onChange }) {
  return (
    <div style={{ display: 'flex', borderBottom: `2px solid ${C.border}`, marginBottom: 18, gap: 0, flexWrap: 'wrap' }}>
      {tabs.map(t => (
        <button key={t.id} onClick={() => onChange(t.id)} style={{
          padding: '7px 16px', fontSize: 12, fontWeight: active === t.id ? 700 : 400,
          color: active === t.id ? C.maroon : C.textSec,
          background: 'none', border: 'none',
          borderBottom: active === t.id ? `2px solid ${C.maroon}` : '2px solid transparent',
          marginBottom: -2, cursor: 'pointer',
        }}>
          {t.label}
        </button>
      ))}
    </div>
  );
}

// ─── Back button ──────────────────────────────────────────────────────────────
function BackBtn({ onClick }) {
  return (
    <button onClick={onClick} style={{
      background: 'none', border: 'none', cursor: 'pointer',
      color: C.maroon, fontSize: 13, fontWeight: 600,
      padding: '0 0 14px', display: 'flex', alignItems: 'center', gap: 4,
    }}>
      ← Back to Results Hub
    </button>
  );
}

// ─── Regen hook ───────────────────────────────────────────────────────────────
// Calls a backend regen endpoint and returns a cache-busting timestamp on success.
function useRegen(sessionId) {
  const [busy,  setBusy]  = useState(false);
  const [error, setError] = useState(null);

  const regen = useCallback(async (endpoint, body) => {
    setBusy(true); setError(null);
    try {
      const res = await fetch(`/api/v1/${sessionId}/analysis/diablo/regen/${endpoint}`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(body),
      });
      const data = await res.json();
      if (!res.ok || data.error_code) {
        setError(data.detail || data.error_code || 'Regeneration failed');
        return null;
      }
      return data.timestamp || String(Date.now());
    } catch (e) {
      setError(String(e.message ?? e));
      return null;
    } finally {
      setBusy(false);
    }
  }, [sessionId]);

  return { regen, busy, error };
}

// ─── Paginated sortable table ─────────────────────────────────────────────────
function DataTable({ rows, columns, pageSize: defaultPageSize = 20 }) {
  const [sortCol,  setSortCol]  = useState(null);
  const [sortDir,  setSortDir]  = useState('asc');
  const [filter,   setFilter]   = useState('');
  const [page,     setPage]     = useState(1);
  const [pageSize, setPageSize] = useState(defaultPageSize);

  const filtered = useMemo(() => {
    if (!filter) return rows;
    const q = filter.toLowerCase();
    return rows.filter(r => columns.some(c => String(r[c.key] ?? '').toLowerCase().includes(q)));
  }, [rows, columns, filter]);

  const sorted = useMemo(() => {
    if (!sortCol) return filtered;
    return [...filtered].sort((a, b) => {
      const av = a[sortCol] ?? '', bv = b[sortCol] ?? '';
      const cmp = typeof av === 'number' ? av - bv : String(av).localeCompare(String(bv));
      return sortDir === 'asc' ? cmp : -cmp;
    });
  }, [filtered, sortCol, sortDir]);

  const totalPages = Math.max(1, Math.ceil(sorted.length / pageSize));
  const safePage   = Math.min(page, totalPages);
  const display    = sorted.slice((safePage - 1) * pageSize, safePage * pageSize);

  function toggleSort(key) {
    if (sortCol === key) setSortDir(d => d === 'asc' ? 'desc' : 'asc');
    else { setSortCol(key); setSortDir('asc'); }
    setPage(1);
  }

  return (
    <div>
      <div style={{ display: 'flex', alignItems: 'center', gap: 10, marginBottom: 8 }}>
        <input placeholder="Filter…" value={filter} onChange={e => { setFilter(e.target.value); setPage(1); }}
          style={{ padding: '4px 9px', fontSize: 12, border: `1px solid ${C.border}`, borderRadius: 4, width: 200 }} />
        <span style={{ fontSize: 11, color: C.textMuted }}>
          {sorted.length} row{sorted.length !== 1 ? 's' : ''}
        </span>
      </div>
      <div style={{ overflowX: 'auto' }}>
        <table style={{ borderCollapse: 'collapse', fontSize: 12, width: '100%' }}>
          <thead>
            <tr>
              {columns.map(c => (
                <th key={c.key} onClick={() => toggleSort(c.key)} style={{
                  padding: '5px 9px', background: C.bg, border: `1px solid ${C.border}`,
                  textAlign: 'left', cursor: 'pointer', fontWeight: 700, color: C.textPrimary, whiteSpace: 'nowrap',
                }}>
                  {c.label} {sortCol === c.key ? (sortDir === 'asc' ? '↑' : '↓') : ''}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {display.map((row, i) => (
              <tr key={i} style={{ background: i % 2 === 0 ? C.white : C.bgLight }}>
                {columns.map(c => (
                  <td key={c.key} style={{ padding: '4px 9px', border: `1px solid ${C.border}`,
                                           color: C.textSec, fontFamily: c.mono ? 'monospace' : 'inherit' }}>
                    {c.format ? c.format(row[c.key]) : String(row[c.key] ?? '')}
                  </td>
                ))}
              </tr>
            ))}
            {display.length === 0 && (
              <tr><td colSpan={columns.length} style={{ padding: 10, color: C.textMuted,
                                                         textAlign: 'center', fontStyle: 'italic' }}>No data</td></tr>
            )}
          </tbody>
        </table>
      </div>
      {totalPages > 1 && (
        <div style={{ display: 'flex', alignItems: 'center', gap: 6, marginTop: 8, fontSize: 12 }}>
          <button onClick={() => setPage(1)}      disabled={safePage === 1}          style={pgBtn}>«</button>
          <button onClick={() => setPage(p => Math.max(1, p - 1))} disabled={safePage === 1} style={pgBtn}>‹</button>
          <span style={{ color: C.textSec }}>Page {safePage} / {totalPages}</span>
          <button onClick={() => setPage(p => Math.min(totalPages, p + 1))} disabled={safePage === totalPages} style={pgBtn}>›</button>
          <button onClick={() => setPage(totalPages)} disabled={safePage === totalPages} style={pgBtn}>»</button>
        </div>
      )}
      <div style={{ marginTop: 6, fontSize: 11, color: C.textMuted }}>
        Rows per page:{' '}
        <select value={pageSize} onChange={e => { setPageSize(Number(e.target.value)); setPage(1); }}
          style={{ fontSize: 11, border: `1px solid ${C.border}`, borderRadius: 3, padding: '1px 4px' }}>
          {[10, 20, 50, 100].map(n => <option key={n} value={n}>{n}</option>)}
        </select>
      </div>
    </div>
  );
}
const pgBtn = {
  padding: '2px 7px', fontSize: 12, border: `1px solid ${C.border}`,
  borderRadius: 3, background: C.bg, cursor: 'pointer', color: C.textSec,
};

// ─── CSV fetch + paginated table ──────────────────────────────────────────────
function parseCsvLine(line) {
  const vals = [];
  let cur = '';
  let inQuote = false;
  for (let i = 0; i < line.length; i++) {
    const ch = line[i];
    if (inQuote) {
      if (ch === '"') {
        if (line[i + 1] === '"') { cur += '"'; i++; }
        else inQuote = false;
      } else { cur += ch; }
    } else {
      if (ch === '"') { inQuote = true; }
      else if (ch === ',') { vals.push(cur.trim()); cur = ''; }
      else { cur += ch; }
    }
  }
  vals.push(cur.trim());
  return vals;
}

function CsvTable({ url, columns, pageSize = 20 }) {
  const [rows,    setRows]    = useState(null);
  const [loading, setLoading] = useState(true);
  const [error,   setError]   = useState(null);

  useEffect(() => {
    if (!url) { setLoading(false); return; }
    fetch(url)
      .then(r => { if (!r.ok) throw new Error(`HTTP ${r.status}`); return r.text(); })
      .then(text => {
        const lines = text.trim().split('\n');
        if (lines.length < 2) { setRows([]); return; }
        const headers = parseCsvLine(lines[0]);
        const parsed  = lines.slice(1).map(line => {
          const vals = parseCsvLine(line);
          const obj  = {};
          headers.forEach((h, i) => { obj[h] = vals[i] ?? ''; });
          return obj;
        });
        setRows(parsed);
      })
      .catch(e => setError(String(e.message ?? e)))
      .finally(() => setLoading(false));
  }, [url]);

  if (!url)    return <Unavailable msg="Table not available for this run." />;
  if (loading) return <p style={{ fontSize: 12, color: C.textMuted }}>Loading table…</p>;
  if (error)   return <p style={{ fontSize: 12, color: '#c0392b' }}>Table error: {error}</p>;
  if (!rows)   return null;

  const cols = columns || (rows.length > 0
    ? Object.keys(rows[0]).map(k => ({ key: k, label: k }))
    : []);

  return <DataTable rows={rows} columns={cols} pageSize={pageSize} />;
}

// ─── Collapsible section ──────────────────────────────────────────────────────
function Collapsible({ label, defaultOpen = false, children }) {
  const [open, setOpen] = useState(defaultOpen);
  return (
    <div style={{ marginTop: 12 }}>
      <button onClick={() => setOpen(o => !o)} style={{
        background: C.bg, border: `1px solid ${C.border}`, borderRadius: 4,
        padding: '5px 12px', fontSize: 12, cursor: 'pointer', color: C.textSec,
        display: 'flex', alignItems: 'center', gap: 6,
      }}>
        <span style={{ fontSize: 10 }}>{open ? '▼' : '▶'}</span>
        {label}
      </button>
      {open && <div style={{ marginTop: 10 }}>{children}</div>}
    </div>
  );
}

// ─── P-value formatter ────────────────────────────────────────────────────────
// Always scientific notation 2 decimal places.
// If p is 0 or below machine precision, show "< 1.00e-16".
function fmtPval(v) {
  if (v == null || v === '' || v === 'NA') return '—';
  const n = Number(v);
  if (isNaN(n)) return '—';
  if (n === 0 || n < 1e-16) return '< 1.00e-16';
  // toExponential(2) gives e.g. "1.23e-4" → normalise to "1.23e-04"
  return n.toExponential(2).replace(/e([+-])(\d)$/, 'e$1' + '0$2');
}

// ─── Expandable feature-centric table (for Circos + Network edge tables) ──────
function ExpandableFeatureTable({ edgeRows, downloadUrl, downloadFilename }) {
  const [expanded, setExpanded] = useState({});
  const [page,     setPage]     = useState(1);
  const [pageSize, setPageSize] = useState(20);
  const [filter,   setFilter]   = useState('');

  const featureMap = useMemo(() => {
    const map = {};
    edgeRows.forEach(row => {
      const key1 = `${row.feature_1}|||${row.block_1}`;
      const key2 = `${row.feature_2}|||${row.block_2}`;
      if (!map[key1]) map[key1] = { feature: row.feature_1, block: row.block_1, connections: [] };
      if (!map[key2]) map[key2] = { feature: row.feature_2, block: row.block_2, connections: [] };
      map[key1].connections.push({ feature: row.feature_2, block: row.block_2,
        correlation_r: row.correlation_r, p_value: row.p_value });
      map[key2].connections.push({ feature: row.feature_1, block: row.block_1,
        correlation_r: row.correlation_r, p_value: row.p_value });
    });
    return map;
  }, [edgeRows]);

  const allFeatures = useMemo(() =>
    Object.values(featureMap).sort((a, b) => b.connections.length - a.connections.length),
  [featureMap]);

  const filtered = useMemo(() => {
    if (!filter) return allFeatures;
    const q = filter.toLowerCase();
    return allFeatures.filter(f =>
      f.feature.toLowerCase().includes(q) || f.block.toLowerCase().includes(q));
  }, [allFeatures, filter]);

  const totalPages = Math.max(1, Math.ceil(filtered.length / pageSize));
  const safePage   = Math.min(page, totalPages);
  const display    = filtered.slice((safePage - 1) * pageSize, safePage * pageSize);

  if (edgeRows.length === 0) return <Unavailable msg="No edge data available." />;

  return (
    <div>
      <div style={{ display: 'flex', alignItems: 'center', gap: 10, marginBottom: 8 }}>
        <input placeholder="Filter feature or block…" value={filter}
          onChange={e => { setFilter(e.target.value); setPage(1); }}
          style={{ padding: '4px 9px', fontSize: 12, border: `1px solid ${C.border}`, borderRadius: 4, width: 220 }} />
        <span style={{ fontSize: 11, color: C.textMuted }}>{filtered.length} features</span>
      </div>
      <div style={{ overflowX: 'auto' }}>
        <table style={{ borderCollapse: 'collapse', fontSize: 12, width: '100%' }}>
          <thead>
            <tr>
              <th style={thStyle}></th>
              <th style={thStyle}>Feature</th>
              <th style={thStyle}>Block</th>
              <th style={thStyle}>Degree</th>
            </tr>
          </thead>
          <tbody>
            {display.map((feat, i) => {
              const key  = `${feat.feature}|||${feat.block}`;
              const open = !!expanded[key];
              return [
                <tr key={key} style={{ background: i % 2 === 0 ? C.white : C.bgLight }}>
                  <td style={{ ...tdStyle, width: 28, textAlign: 'center', cursor: 'pointer' }}
                      onClick={() => setExpanded(e => ({ ...e, [key]: !e[key] }))}>
                    <span style={{ fontSize: 9 }}>{open ? '▼' : '▶'}</span>
                  </td>
                  <td style={{ ...tdStyle, fontFamily: 'monospace' }}>{feat.feature}</td>
                  <td style={tdStyle}>{feat.block}</td>
                  <td style={tdStyle}>{feat.connections.length}</td>
                </tr>,
                open && (
                  <tr key={key + '_exp'}>
                    <td colSpan={4} style={{ padding: '0 0 0 28px', background: '#f0f0f5', border: `1px solid ${C.border}` }}>
                      <table style={{ borderCollapse: 'collapse', fontSize: 11, width: '100%', margin: '6px 0' }}>
                        <thead>
                          <tr>
                            <th style={subThStyle}>Connected Feature</th>
                            <th style={subThStyle}>Block</th>
                            <th style={subThStyle}>Correlation r</th>
                            <th style={subThStyle}>p-value</th>
                          </tr>
                        </thead>
                        <tbody>
                          {feat.connections.map((conn, ci) => (
                            <tr key={ci} style={{ background: ci % 2 === 0 ? C.white : C.bgLight }}>
                              <td style={{ ...tdStyle, fontFamily: 'monospace', fontSize: 11 }}>{conn.feature}</td>
                              <td style={{ ...tdStyle, fontSize: 11 }}>{conn.block}</td>
                              <td style={{ ...tdStyle, fontSize: 11 }}>{isNaN(Number(conn.correlation_r)) ? conn.correlation_r : Number(conn.correlation_r).toFixed(4)}</td>
                              <td style={{ ...tdStyle, fontSize: 11, fontFamily: 'monospace' }}>{fmtPval(conn.p_value)}</td>
                            </tr>
                          ))}
                        </tbody>
                      </table>
                    </td>
                  </tr>
                ),
              ];
            })}
          </tbody>
        </table>
      </div>
      {totalPages > 1 && (
        <div style={{ display: 'flex', alignItems: 'center', gap: 6, marginTop: 8, fontSize: 12 }}>
          <button onClick={() => setPage(1)}      disabled={safePage === 1}          style={pgBtn}>«</button>
          <button onClick={() => setPage(p => Math.max(1, p - 1))} disabled={safePage === 1} style={pgBtn}>‹</button>
          <span style={{ color: C.textSec }}>Page {safePage} / {totalPages}</span>
          <button onClick={() => setPage(p => Math.min(totalPages, p + 1))} disabled={safePage === totalPages} style={pgBtn}>›</button>
          <button onClick={() => setPage(totalPages)} disabled={safePage === totalPages} style={pgBtn}>»</button>
        </div>
      )}
      <div style={{ marginTop: 6, fontSize: 11, color: C.textMuted }}>
        Rows per page:{' '}
        <select value={pageSize} onChange={e => { setPageSize(Number(e.target.value)); setPage(1); }}
          style={{ fontSize: 11, border: `1px solid ${C.border}`, borderRadius: 3, padding: '1px 4px' }}>
          {[10, 20, 50, 100].map(n => <option key={n} value={n}>{n}</option>)}
        </select>
      </div>
      {downloadUrl && (
        <div style={{ marginTop: 8 }}>
          <DownloadBtn label="CSV (full edge list)" url={downloadUrl} filename={downloadFilename} />
        </div>
      )}
    </div>
  );
}
const thStyle    = { padding: '5px 9px', background: C.bg, border: `1px solid ${C.border}`, textAlign: 'left', fontWeight: 700, color: C.textPrimary, whiteSpace: 'nowrap' };
const subThStyle = { padding: '4px 8px', background: '#e8e8f0', border: `1px solid ${C.border}`, textAlign: 'left', fontWeight: 600, color: C.textPrimary };
const tdStyle    = { padding: '4px 9px', border: `1px solid ${C.border}`, color: C.textSec };

// ─── CSV-backed expandable feature table ──────────────────────────────────────
function CsvExpandableTable({ url, downloadFilename, filterFn = null, cutoff = null }) {
  const [rows,    setRows]    = useState(null);
  const [loading, setLoading] = useState(true);
  const [error,   setError]   = useState(null);

  useEffect(() => {
    if (!url) { setLoading(false); return; }
    setLoading(true); setRows(null); setError(null);
    fetch(url)
      .then(r => { if (!r.ok) throw new Error(`HTTP ${r.status}`); return r.text(); })
      .then(text => {
        const lines = text.trim().split('\n');
        if (lines.length < 2) { setRows([]); return; }
        const headers = parseCsvLine(lines[0]);
        const parsed  = lines.slice(1).map(line => {
          const vals = parseCsvLine(line);
          const obj  = {};
          headers.forEach((h, i) => { obj[h] = vals[i] ?? ''; });
          return obj;
        });
        setRows(parsed);
      })
      .catch(e => setError(String(e.message ?? e)))
      .finally(() => setLoading(false));
  }, [url]);

  if (!url)    return <Unavailable msg="Table not available for this run." />;
  if (loading) return <p style={{ fontSize: 12, color: C.textMuted }}>Loading table…</p>;
  if (error)   return <p style={{ fontSize: 12, color: '#c0392b' }}>Table error: {error}</p>;
  if (!rows || rows.length === 0) return <Unavailable msg="No edge data available." />;

  let displayRows = filterFn ? rows.filter(filterFn) : rows;
  if (cutoff != null && isFinite(cutoff)) {
    displayRows = displayRows.filter(r => Math.abs(Number(r.correlation_r)) >= cutoff);
  }
  return <ExpandableFeatureTable edgeRows={displayRows} downloadUrl={url} downloadFilename={downloadFilename} />;
}

// ─── SVG icons ────────────────────────────────────────────────────────────────
const Icons = {
  multiblock: (
    <svg width="22" height="22" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="1.8" strokeLinecap="round" strokeLinejoin="round">
      <circle cx="12" cy="12" r="10"/><line x1="12" y1="8" x2="12" y2="16"/><line x1="8" y1="12" x2="16" y2="12"/>
    </svg>
  ),
  blockspecific: (
    <svg width="22" height="22" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="1.8" strokeLinecap="round" strokeLinejoin="round">
      <rect x="3" y="3" width="7" height="7"/><rect x="14" y="3" width="7" height="7"/>
      <rect x="3" y="14" width="7" height="7"/><rect x="14" y="14" width="7" height="7"/>
    </svg>
  ),
  circos: (
    <svg width="22" height="22" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="1.8" strokeLinecap="round" strokeLinejoin="round">
      <circle cx="12" cy="12" r="9"/>
      <path d="M12 3 C16 7 16 17 12 21"/><path d="M12 3 C8 7 8 17 12 21"/>
    </svg>
  ),
  network: (
    <svg width="22" height="22" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="1.8" strokeLinecap="round" strokeLinejoin="round">
      <circle cx="12" cy="5" r="2"/><circle cx="5" cy="19" r="2"/><circle cx="19" cy="19" r="2"/>
      <line x1="12" y1="7" x2="5" y2="17"/><line x1="12" y1="7" x2="19" y2="17"/><line x1="5" y1="19" x2="19" y2="19"/>
    </svg>
  ),
  performance: (
    <svg width="22" height="22" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="1.8" strokeLinecap="round" strokeLinejoin="round">
      <polyline points="22 12 18 12 15 21 9 3 6 12 2 12"/>
    </svg>
  ),
  export: (
    <svg width="22" height="22" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="1.8" strokeLinecap="round" strokeLinejoin="round">
      <path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"/>
      <polyline points="7 10 12 15 17 10"/><line x1="12" y1="15" x2="12" y2="3"/>
    </svg>
  ),
};

// ─── Nav card ─────────────────────────────────────────────────────────────────
function NavCard({ label, icon, onClick, available }) {
  return (
    <button onClick={onClick} style={{
      display: 'flex', flexDirection: 'column', alignItems: 'flex-start',
      gap: 6, padding: '14px 16px', width: '100%',
      background: available ? C.white : C.bgLight,
      border: `1px solid ${available ? C.maroon : C.border}`,
      borderRadius: 6, cursor: 'pointer',
      opacity: available ? 1 : 0.55, textAlign: 'left',
    }}>
      <span style={{ color: available ? C.maroon : C.textMuted }}>{icon}</span>
      <span style={{ fontSize: 13, fontWeight: 600, color: available ? C.maroon : C.textMuted }}>
        {label}
      </span>
      {!available && <span style={{ fontSize: 11, color: C.textMuted }}>Not generated</span>}
    </button>
  );
}

// ─── Regen status bar ─────────────────────────────────────────────────────────
function RegenStatus({ busy, error }) {
  if (!busy && !error) return null;
  return (
    <div style={{ marginTop: 8, fontSize: 12,
                  color: error ? '#c0392b' : C.textMuted,
                  fontStyle: error ? 'normal' : 'italic' }}>
      {busy ? 'Regenerating plot…' : `Error: ${error}`}
    </div>
  );
}

// ─── Apply button ─────────────────────────────────────────────────────────────
function ApplyBtn({ onClick, busy, label = 'Apply' }) {
  return (
    <button onClick={onClick} disabled={busy} style={{
      padding: '3px 12px', fontSize: 12, background: busy ? C.bg : C.maroon,
      color: busy ? C.textMuted : '#fff', border: `1px solid ${busy ? C.border : C.maroon}`,
      borderRadius: 4, cursor: busy ? 'not-allowed' : 'pointer', fontWeight: 600,
    }}>
      {busy ? '…' : label}
    </button>
  );
}

// ─── Page: Results Hub ────────────────────────────────────────────────────────
function HubPage({ summary, manifest, onNavigate, onRunAgain }) {
  const warnings = summary.warnings || [];
  const mEntries = manifest ? Object.values(manifest) : [];

  function hasPage(page) {
    return mEntries.some(a => a.page === page && a.status === 'success');
  }

  const resultPages = [
    { id: 'multiblock',    label: 'Multi-Block sPLS-DA',    icon: Icons.multiblock },
    { id: 'blockspecific', label: 'Block-Specific sPLS-DAs', icon: Icons.blockspecific },
    { id: 'circos',        label: 'Circos',                  icon: Icons.circos },
    { id: 'network',       label: 'Network',                 icon: Icons.network },
    { id: 'performance',   label: 'Performance',             icon: Icons.performance },
  ];

  return (
    <div style={{ maxWidth: 860 }}>
      <h2 style={{ fontSize: 19, fontWeight: 700, color: C.textPrimary, margin: '0 0 18px' }}>
        View Results
      </h2>

      {warnings.length > 0 ? (
        <div style={{ background: C.amberBg, border: `1px solid ${C.amberBorder}`,
                      borderRadius: 5, padding: '8px 14px', marginBottom: 18, fontSize: 13 }}>
          <span style={{ fontWeight: 600, color: C.amber }}>⚠ Analysis completed with warnings</span>
          <ul style={{ margin: '4px 0 0', paddingLeft: 18, color: C.textMuted, fontSize: 12 }}>
            {warnings.map((w, i) => <li key={i}>{w}</li>)}
          </ul>
        </div>
      ) : (
        <div style={{ background: C.greenBg, border: `1px solid ${C.greenBorder}`,
                      borderRadius: 5, padding: '7px 14px', marginBottom: 18,
                      fontSize: 13, color: C.green, fontWeight: 600 }}>
          ✓ Analysis completed successfully
        </div>
      )}

      <div style={{ marginBottom: 22 }}>
        <SectionTitle>Run Summary</SectionTitle>
        <div style={{ display: 'flex', flexWrap: 'wrap', gap: 8 }}>
          {summary.selected_groups?.length > 0 && (
            <Chip label="Groups" value={summary.selected_groups.join(' vs ')} />
          )}
          {summary.ncomp_used != null && <Chip label="Components" value={summary.ncomp_used} />}
          {summary.block_names?.length > 0 && <Chip label="Blocks" value={summary.block_names.join(', ')} />}
          {summary.keepX_grid_used?.length > 0 && <Chip label="keepX grid" value={summary.keepX_grid_used.join(', ')} />}
          {summary.seed_used != null && <Chip label="Seed" value={summary.seed_used} mono />}
        </div>
        {summary.keepX_chosen && Object.keys(summary.keepX_chosen).length > 0 && (
          <div style={{ marginTop: 8, fontSize: 12, color: C.textSec }}>
            Tuned keepX:{' '}
            {Object.entries(summary.keepX_chosen).map(([block, vals]) =>
              `${block}: [${Array.isArray(vals) ? vals.join(', ') : vals}]`
            ).join(' · ')}
          </div>
        )}
      </div>

      <div style={{ marginBottom: 32 }}>
        <SectionTitle>Results Sections</SectionTitle>
        <div style={{ display: 'grid', gridTemplateColumns: 'repeat(3, 1fr)', gap: 12 }}>
          {resultPages.map(p => (
            <NavCard key={p.id} label={p.label} icon={p.icon}
              available={p.id === 'performance' || hasPage(p.id === 'multiblock' ? 'consensus' : p.id === 'blockspecific' ? 'components' : p.id)}
              onClick={() => onNavigate(p.id)} />
          ))}
          <NavCard label="Export Results" icon={Icons.export} available onClick={() => onNavigate('export')} />
        </div>
      </div>

      {manifest && (() => {
        const expectedSkips = new Set(['consensus_loadings']);
        const failed = Object.values(manifest).filter(a =>
          (a.status === 'failed' || a.status === 'skipped') && !expectedSkips.has(a.artifact_key)
        );
        if (failed.length === 0) return null;
        return (
          <div style={{ background: C.amberBg, border: `1px solid ${C.amberBorder}`,
                        borderRadius: 5, padding: '8px 12px', fontSize: 12, marginBottom: 18 }}>
            <div style={{ fontWeight: 600, color: C.amber, marginBottom: 4 }}>
              ⚠ Some optional artifacts were not generated
            </div>
            <ul style={{ margin: 0, paddingLeft: 18, color: C.textMuted }}>
              {failed.map(a => (
                <li key={a.artifact_key}>{a.display_name}{a.message ? ` — ${a.message}` : ''}</li>
              ))}
            </ul>
          </div>
        );
      })()}

      {onRunAgain && (
        <button onClick={onRunAgain} style={{
          padding: '7px 18px', background: C.textSec, color: '#fff',
          border: 'none', borderRadius: 5, cursor: 'pointer', fontWeight: 600, fontSize: 13,
        }}>
          ← Reset & Reconfigure
        </button>
      )}
    </div>
  );
}

// ─── Scores customization panel ───────────────────────────────────────────────
// Used by both MultiBlockPage and BlockSpecificPage.
function ScoresCustomPanel({ sessionId, plotId, groupLevels, onRegenDone }) {
  const DEFAULT_PALETTE = ['#388ECC','#F68B33','#C2C2C2','#009E73','#CC79A7',
                           '#56B4E9','#E69F00','#D55E00','#0072B2','#F0E442'];
  const initColors = Object.fromEntries(
    groupLevels.map((g, i) => [g, DEFAULT_PALETTE[i % DEFAULT_PALETTE.length]])
  );
  const [colors,      setColors]      = useState(initColors);
  const [showLabels,  setShowLabels]  = useState(false);
  const { regen, busy, error }        = useRegen(sessionId);

  async function handleApply() {
    const ts = await regen('scores', { plot_id: plotId, colors, show_labels: showLabels });
    if (ts) onRegenDone(ts);
  }

  return (
    <div style={{ marginTop: 14, padding: '10px 14px', background: C.bgLight,
                  border: `1px solid ${C.border}`, borderRadius: 5 }}>
      <div style={{ fontSize: 12, fontWeight: 700, color: C.textPrimary, marginBottom: 8 }}>
        Plot Customization
      </div>
      <div style={{ display: 'flex', flexWrap: 'wrap', gap: 10, alignItems: 'center', marginBottom: 8 }}>
        {groupLevels.map(g => (
          <label key={g} style={{ fontSize: 12, color: C.textSec, display: 'flex', alignItems: 'center', gap: 5 }}>
            <span style={{ width: 14, height: 14, borderRadius: '50%',
                           background: colors[g] || '#888', display: 'inline-block',
                           border: '1px solid #ccc' }} />
            {g}
            <input type="color" value={colors[g] || '#888888'}
              onChange={e => setColors(c => ({ ...c, [g]: e.target.value }))}
              style={{ width: 28, height: 22, padding: 0, border: 'none', cursor: 'pointer' }} />
          </label>
        ))}
      </div>
      <div style={{ display: 'flex', alignItems: 'center', gap: 14 }}>
        <label style={{ fontSize: 12, color: C.textSec, display: 'flex', alignItems: 'center', gap: 6, cursor: 'pointer' }}>
          <input type="checkbox" checked={showLabels} onChange={e => setShowLabels(e.target.checked)} />
          Show sample labels
        </label>
        <ApplyBtn onClick={handleApply} busy={busy} label="Update Plot" />
      </div>
      <RegenStatus busy={busy} error={error} />
    </div>
  );
}

// ─── Page: Multi-Block sPLS-DA ────────────────────────────────────────────────
function MultiBlockPage({ manifest, summary, sessionId, onBack }) {
  const [tab,       setTab]       = useState('multiblock_scores');
  const [cacheBust, setCacheBust] = useState({});
  const [topNLoad,  setTopNLoad]  = useState('40');
  const [topNVip,   setTopNVip]   = useState('40');
  const { regen: regenLoad, busy: busyLoad, error: errLoad } = useRegen(sessionId);
  const { regen: regenVip,  busy: busyVip,  error: errVip  } = useRegen(sessionId);

  const mEntries = manifest ? Object.values(manifest) : [];
  const scoresArt   = mEntries.find(a => a.tab_id === 'multiblock_scores'        && a.status === 'success') || null;
  const loadingsArt = mEntries.find(a => a.tab_id === 'consensus_loadings'       && a.status === 'success') || null;
  const loadingsCsv = mEntries.find(a => a.tab_id === 'consensus_loadings_table' && a.status === 'success') || null;
  const vipArt      = mEntries.find(a => a.tab_id === 'consensus_vip'            && a.status === 'success') || null;
  const vipCsv      = mEntries.find(a => a.tab_id === 'consensus_vip_table'      && a.status === 'success') || null;

  const groupLevels = summary?.selected_groups || [];

  const tabs = [
    { id: 'multiblock_scores',  label: '2D Scores Plot' },
    { id: 'consensus_loadings', label: 'Consensus Loadings' },
    { id: 'consensus_vip',      label: 'Consensus VIP' },
  ];

  async function handleRegenLoad() {
    const n = Math.max(1, parseInt(topNLoad, 10) || 40);
    const ts = await regenLoad('consensus-loadings', { top_n: n });
    if (ts) setCacheBust(b => ({ ...b, consensus_loadings: ts }));
  }

  async function handleRegenVip() {
    const n = Math.max(1, parseInt(topNVip, 10) || 40);
    const ts = await regenVip('consensus-vip', { top_n: n });
    if (ts) setCacheBust(b => ({ ...b, consensus_vip: ts }));
  }

  return (
    <div style={{ maxWidth: 900 }}>
      <BackBtn onClick={onBack} />
      <h2 style={{ fontSize: 17, fontWeight: 700, color: C.textPrimary, margin: '0 0 18px' }}>
        Multi-Block sPLS-DA
      </h2>
      <TabBar tabs={tabs} active={tab} onChange={setTab} />

      {tab === 'multiblock_scores' && (
        scoresArt ? (
          <div>
            <PlotImg url={scoresArt.png_url} alt="2D Scores Plot - Multi-Block sPLS-DA"
                     cacheBust={cacheBust['multiblock_scores']} />
            <div style={{ display: 'flex', gap: 8, marginTop: 10 }}>
              <DownloadBtn label="PNG" url={scoresArt.png_url} filename="multiblock_scores.png" />
              <DownloadBtn label="PDF" url={scoresArt.pdf_url} filename="multiblock_scores.pdf" />
            </div>
            <div style={{ marginTop: 12, fontSize: 12, color: C.textMuted, fontStyle: 'italic' }}>
              Weighted-average multiblock sample scores. Points colored by group.
            </div>
            {groupLevels.length > 0 && (
              <ScoresCustomPanel
                sessionId={sessionId}
                plotId="multiblock_scores"
                groupLevels={groupLevels}
                onRegenDone={ts => setCacheBust(b => ({ ...b, multiblock_scores: ts }))}
              />
            )}
          </div>
        ) : <Unavailable msg="Multi-block 2D scores plot was not generated for this run." />
      )}

      {tab === 'consensus_loadings' && (
        loadingsArt ? (
          <div>
            <PlotImg url={loadingsArt.png_url} alt="Loadings Plot - Multi-Block sPLS-DA"
                     cacheBust={cacheBust['consensus_loadings']} />
            <div style={{ display: 'flex', gap: 8, marginTop: 10 }}>
              <DownloadBtn label="PNG" url={loadingsArt.png_url} filename="consensus_loadings.png" />
              <DownloadBtn label="PDF" url={loadingsArt.pdf_url} filename="consensus_loadings.pdf" />
            </div>
            {/* Top N control */}
            <div style={{ marginTop: 12, padding: '8px 12px', background: C.bgLight,
                          border: `1px solid ${C.border}`, borderRadius: 5,
                          display: 'flex', alignItems: 'center', gap: 10, flexWrap: 'wrap' }}>
              <label style={{ fontSize: 12, color: C.textSec }}>Top N features displayed:</label>
              <input type="number" min="1" max="200" value={topNLoad}
                onChange={e => setTopNLoad(e.target.value)}
                style={{ width: 60, padding: '3px 6px', fontSize: 12,
                         border: `1px solid ${C.border}`, borderRadius: 4 }} />
              <ApplyBtn onClick={handleRegenLoad} busy={busyLoad} label="Update Plot" />
              <RegenStatus busy={busyLoad} error={errLoad} />
            </div>
            {loadingsCsv && (
              <Collapsible label="Show Consensus Loadings Table" defaultOpen={false}>
                <CsvTable
                  url={loadingsCsv.csv_url}
                  columns={[
                    { key: 'feature',                label: 'Feature' },
                    { key: 'block',                  label: 'Block' },
                    { key: 'consensus_contribution', label: 'Consensus Contribution', format: v => isNaN(Number(v)) ? v : Number(v).toFixed(4) },
                    { key: 'block_weight_comp1',     label: 'Block Weight',           format: v => isNaN(Number(v)) ? v : Number(v).toFixed(4) },
                    { key: 'loading_comp1',          label: 'Loading (comp 1)',        format: v => isNaN(Number(v)) ? v : Number(v).toFixed(4) },
                  ]}
                  pageSize={20}
                />
                <div style={{ marginTop: 8 }}>
                  <DownloadBtn label="CSV" url={loadingsCsv.csv_url} filename="consensus_loadings.csv" />
                </div>
              </Collapsible>
            )}
          </div>
        ) : (
          <Unavailable msg="Consensus loadings were not generated for this run." />
        )
      )}

      {tab === 'consensus_vip' && (
        vipArt ? (
          <div>
            <PlotImg url={vipArt.png_url} alt="Consensus VIP - Multi-Block sPLS-DA"
                     cacheBust={cacheBust['consensus_vip']} />
            <div style={{ display: 'flex', gap: 8, marginTop: 10 }}>
              <DownloadBtn label="PNG" url={vipArt.png_url} filename="consensus_vip.png" />
              <DownloadBtn label="PDF" url={vipArt.pdf_url} filename="consensus_vip.pdf" />
            </div>
            {/* Top N control */}
            <div style={{ marginTop: 12, padding: '8px 12px', background: C.bgLight,
                          border: `1px solid ${C.border}`, borderRadius: 5,
                          display: 'flex', alignItems: 'center', gap: 10, flexWrap: 'wrap' }}>
              <label style={{ fontSize: 12, color: C.textSec }}>Top N features displayed:</label>
              <input type="number" min="1" max="200" value={topNVip}
                onChange={e => setTopNVip(e.target.value)}
                style={{ width: 60, padding: '3px 6px', fontSize: 12,
                         border: `1px solid ${C.border}`, borderRadius: 4 }} />
              <ApplyBtn onClick={handleRegenVip} busy={busyVip} label="Update Plot" />
              <RegenStatus busy={busyVip} error={errVip} />
            </div>
            <div style={{ marginTop: 8, fontSize: 12, color: C.textMuted, fontStyle: 'italic' }}>
              Consensus VIP = block_weight × per-block VIP (comp 1). Higher = greater contribution.
            </div>
            {(vipCsv?.csv_url) && (
              <Collapsible label="Show Consensus VIP Table" defaultOpen={false}>
                <CsvTable
                  url={vipCsv.csv_url}
                  columns={[
                    { key: 'feature',       label: 'Feature' },
                    { key: 'block',         label: 'Block' },
                    { key: 'consensus_vip', label: 'Consensus VIP',  format: v => isNaN(Number(v)) ? v : Number(v).toFixed(4) },
                    { key: 'block_weight',  label: 'Block Weight',   format: v => isNaN(Number(v)) ? v : Number(v).toFixed(4) },
                    { key: 'vip_comp1',     label: 'VIP (comp 1)',   format: v => isNaN(Number(v)) ? v : Number(v).toFixed(4) },
                  ]}
                  pageSize={20}
                />
                <div style={{ marginTop: 8 }}>
                  <DownloadBtn label="CSV" url={vipCsv.csv_url} filename="consensus_vip.csv" />
                </div>
              </Collapsible>
            )}
          </div>
        ) : (
          <Unavailable msg="Consensus VIP was not generated for this run." />
        )
      )}
    </div>
  );
}

// ─── Page: Block-Specific sPLS-DAs ────────────────────────────────────────────
function BlockSpecificPage({ manifest, summary, sessionId, onBack }) {
  const mEntries   = manifest ? Object.values(manifest) : [];
  const blockNames = summary?.block_names || [];
  const groupLevels = summary?.selected_groups || [];

  const allTabs = blockNames.flatMap(b => {
    const safe = b.replace(/[^A-Za-z0-9_]/g, '_');
    return [
      { id: `block_${safe}_scores`,   label: `${b} — 2D Scores Plot` },
      { id: `block_${safe}_loadings`, label: `${b} — Loadings` },
    ];
  });

  const [tab,       setTab]       = useState(allTabs[0]?.id || '');
  const [cacheBust, setCacheBust] = useState({});
  const [topNMap,   setTopNMap]   = useState({});
  const { regen, busy, error }    = useRegen(sessionId);

  if (allTabs.length === 0) {
    return (
      <div style={{ maxWidth: 900 }}>
        <BackBtn onClick={onBack} />
        <Unavailable msg="No block information available." />
      </div>
    );
  }

  const activeBlock = blockNames.find(b => {
    const safe = b.replace(/[^A-Za-z0-9_]/g, '_');
    return tab === `block_${safe}_scores` || tab === `block_${safe}_loadings`;
  }) || blockNames[0];
  const safeActive    = activeBlock.replace(/[^A-Za-z0-9_]/g, '_');
  const isScoresTab   = tab === `block_${safeActive}_scores`;
  const isLoadingsTab = tab === `block_${safeActive}_loadings`;

  const scoresArt = mEntries.find(a =>
    a.tab_id === `block_${safeActive}_scores` && a.status === 'success'
  ) || null;
  const loadingsPlot = mEntries.find(a =>
    a.tab_id === `block_${safeActive}_loadings` && a.artifact_type === 'plot' && a.status === 'success'
  ) || null;
  const loadingsTable = mEntries.find(a =>
    a.tab_id === `block_${safeActive}_loadings` && a.artifact_type === 'table' && a.status === 'success'
  ) || null;

  const topN = topNMap[safeActive] ?? '30';

  async function handleRegenLoadings() {
    const n = Math.max(1, parseInt(topN, 10) || 30);
    const plotId = `block_${safeActive}_loadings`;
    const ts = await regen('loadings', { plot_id: plotId, top_n: n });
    if (ts) setCacheBust(b => ({ ...b, [plotId]: ts }));
  }

  return (
    <div style={{ maxWidth: 900 }}>
      <BackBtn onClick={onBack} />
      <h2 style={{ fontSize: 17, fontWeight: 700, color: C.textPrimary, margin: '0 0 18px' }}>
        Block-Specific sPLS-DAs
      </h2>
      <TabBar tabs={allTabs} active={tab} onChange={setTab} />

      {isScoresTab && (
        scoresArt ? (
          <div>
            <PlotImg url={scoresArt.png_url} alt={`2D Scores Plot - ${activeBlock}`}
                     cacheBust={cacheBust[`block_${safeActive}_scores`]} />
            <div style={{ display: 'flex', gap: 8, marginTop: 10 }}>
              <DownloadBtn label="PNG" url={scoresArt.png_url} filename={`block_${safeActive}_scores.png`} />
              <DownloadBtn label="PDF" url={scoresArt.pdf_url} filename={`block_${safeActive}_scores.pdf`} />
            </div>
            {groupLevels.length > 0 && (
              <ScoresCustomPanel
                sessionId={sessionId}
                plotId={`block_${safeActive}_scores`}
                groupLevels={groupLevels}
                onRegenDone={ts => setCacheBust(b => ({ ...b, [`block_${safeActive}_scores`]: ts }))}
              />
            )}
          </div>
        ) : <Unavailable msg={`2D scores plot for "${activeBlock}" was not generated for this run.`} />
      )}

      {isLoadingsTab && (
        <div>
          {loadingsPlot ? (
            <div>
              <PlotImg url={loadingsPlot.png_url} alt={`Loadings Plot - ${activeBlock}`}
                       cacheBust={cacheBust[`block_${safeActive}_loadings`]} />
              <div style={{ display: 'flex', gap: 8, marginTop: 10 }}>
                <DownloadBtn label="PNG" url={loadingsPlot.png_url} filename={`block_${safeActive}_loadings.png`} />
                <DownloadBtn label="PDF" url={loadingsPlot.pdf_url} filename={`block_${safeActive}_loadings.pdf`} />
              </div>
              {/* Top N control */}
              <div style={{ marginTop: 12, padding: '8px 12px', background: C.bgLight,
                            border: `1px solid ${C.border}`, borderRadius: 5,
                            display: 'flex', alignItems: 'center', gap: 10, flexWrap: 'wrap' }}>
                <label style={{ fontSize: 12, color: C.textSec }}>Top N features displayed:</label>
                <input type="number" min="1" max="200" value={topN}
                  onChange={e => setTopNMap(m => ({ ...m, [safeActive]: e.target.value }))}
                  style={{ width: 60, padding: '3px 6px', fontSize: 12,
                           border: `1px solid ${C.border}`, borderRadius: 4 }} />
                <ApplyBtn onClick={handleRegenLoadings} busy={busy} label="Update Plot" />
                <RegenStatus busy={busy} error={error} />
              </div>
            </div>
          ) : (
            <Unavailable msg={`Loadings plot for "${activeBlock}" was not generated for this run.`} />
          )}
          {loadingsTable && (
            <div style={{ marginTop: 18 }}>
              <SectionTitle>Loadings Table — {activeBlock}</SectionTitle>
              <CsvTable
                url={loadingsTable.csv_url}
                columns={[
                  { key: 'feature',     label: 'Feature' },
                  { key: 'block',       label: 'Block' },
                  { key: 'loading',     label: 'Loading',   format: v => isNaN(Number(v)) ? v : Number(v).toFixed(4) },
                  { key: 'abs_loading', label: '|Loading|', format: v => isNaN(Number(v)) ? v : Number(v).toFixed(4) },
                  { key: 'sign',        label: 'Sign' },
                  { key: 'rank',        label: 'Rank' },
                  { key: 'selected',    label: 'Selected' },
                ]}
                pageSize={20}
              />
              <div style={{ marginTop: 8 }}>
                <DownloadBtn label="CSV" url={loadingsTable.csv_url} filename={`block_${safeActive}_loadings.csv`} />
              </div>
            </div>
          )}
        </div>
      )}
    </div>
  );
}

// ─── Page: Circos ─────────────────────────────────────────────────────────────
function CircosPage({ manifest, sessionId, onBack }) {
  const [tab,        setTab]        = useState('circos_plot');
  const [crossOnly,  setCrossOnly]  = useState(false);
  const [cutoffStr,  setCutoffStr]  = useState('0.3');
  const [cutoff,     setCutoff]     = useState(0.3);
  const [cacheBust,  setCacheBust]  = useState(null);
  const { regen, busy, error }      = useRegen(sessionId);
  const mEntries = manifest ? Object.values(manifest) : [];

  const plotArt  = mEntries.find(a => a.tab_id === 'circos_plot'  && a.status === 'success') || null;
  const tableArt = mEntries.find(a => a.tab_id === 'circos_table' && a.status === 'success') || null;

  const tabs = [
    { id: 'circos_plot',  label: 'Circos Plot' },
    { id: 'circos_table', label: 'Correlation Table' },
  ];

  async function handleApply() {
    const v = parseFloat(cutoffStr);
    const c = (!isNaN(v) && v >= 0 && v <= 1) ? v : 0.3;
    setCutoff(c);
    // Regenerate the plot PNG with the new cutoff
    const ts = await regen('circos', { cutoff: c });
    if (ts) setCacheBust(ts);
  }

  return (
    <div style={{ maxWidth: 900 }}>
      <BackBtn onClick={onBack} />
      <h2 style={{ fontSize: 17, fontWeight: 700, color: C.textPrimary, margin: '0 0 18px' }}>Circos</h2>
      <TabBar tabs={tabs} active={tab} onChange={setTab} />

      {/* Shared cutoff controls — affect both plot and table */}
      <div style={{ display: 'flex', alignItems: 'center', gap: 14, marginBottom: 14, flexWrap: 'wrap',
                    padding: '8px 12px', background: C.bgLight, border: `1px solid ${C.border}`, borderRadius: 5 }}>
        <div style={{ display: 'flex', alignItems: 'center', gap: 6 }}>
          <label style={{ fontSize: 12, color: C.textSec }}>Correlation cutoff |r| ≥</label>
          <input
            type="number" min="0" max="1" step="0.05"
            value={cutoffStr}
            onChange={e => setCutoffStr(e.target.value)}
            style={{ width: 64, padding: '3px 6px', fontSize: 12, border: `1px solid ${C.border}`, borderRadius: 4 }}
          />
          <ApplyBtn onClick={handleApply} busy={busy} label="Apply" />
          <span style={{ fontSize: 11, color: C.textMuted }}>current: {cutoff.toFixed(2)}</span>
        </div>
        <label style={{ fontSize: 12, color: C.textSec, display: 'flex', alignItems: 'center', gap: 6, cursor: 'pointer' }}>
          <input type="checkbox" checked={crossOnly} onChange={e => setCrossOnly(e.target.checked)} />
          Table: cross-block only
        </label>
        <RegenStatus busy={busy} error={error} />
      </div>

      {tab === 'circos_plot' && (
        plotArt ? (
          <div>
            <PlotImg url={plotArt.png_url} alt="Circos Plot" cacheBust={cacheBust} />
            <div style={{ display: 'flex', gap: 8, marginTop: 10 }}>
              <DownloadBtn label="PNG" url={plotArt.png_url} filename="circos.png" />
              <DownloadBtn label="PDF" url={plotArt.pdf_url} filename="circos.pdf" />
            </div>
            <div style={{ marginTop: 8, fontSize: 11, color: C.textMuted, fontStyle: 'italic' }}>
              Blue = positive correlation, Red = negative correlation.
            </div>
          </div>
        ) : <Unavailable msg="Circos plot was not generated for this run." />
      )}

      {tab === 'circos_table' && (
        tableArt ? (
          <CsvExpandableTable
            url={tableArt.csv_url}
            downloadFilename="circos_edges.csv"
            filterFn={crossOnly ? (r => r.within_block === 'FALSE' || r.within_block === false) : null}
            cutoff={cutoff}
          />
        ) : <Unavailable msg="Circos edge table was not generated for this run." />
      )}
    </div>
  );
}

// ─── Page: Network ────────────────────────────────────────────────────────────
function NetworkPage({ manifest, sessionId, onBack }) {
  const [tab,        setTab]        = useState('network_plot');
  const [crossOnly,  setCrossOnly]  = useState(false);
  const [cutoffStr,  setCutoffStr]  = useState('0.0');
  const [cutoff,     setCutoff]     = useState(0.0);
  const [cacheBust,  setCacheBust]  = useState(null);
  const { regen, busy, error }      = useRegen(sessionId);
  const mEntries = manifest ? Object.values(manifest) : [];

  const plotArt = mEntries.find(a => a.tab_id === 'network_plot' && a.status === 'success') || null;
  const nodeArt = mEntries.find(a => a.tab_id === 'node_metrics' && a.status === 'success') || null;
  const edgeArt = mEntries.find(a => a.tab_id === 'edge_table'   && a.status === 'success') || null;

  const tabs = [
    { id: 'network_plot', label: 'Network Plot' },
    { id: 'node_metrics', label: 'Node Metrics' },
    { id: 'edge_table',   label: 'Edge Table' },
  ];

  async function handleApply() {
    const v = parseFloat(cutoffStr);
    const c = (!isNaN(v) && v >= 0 && v <= 1) ? v : 0.0;
    setCutoff(c);
    const ts = await regen('network', { cutoff: c });
    if (ts) setCacheBust(ts);
  }

  return (
    <div style={{ maxWidth: 900 }}>
      <BackBtn onClick={onBack} />
      <h2 style={{ fontSize: 17, fontWeight: 700, color: C.textPrimary, margin: '0 0 18px' }}>Network</h2>
      <TabBar tabs={tabs} active={tab} onChange={setTab} />

      {/* Shared cutoff controls — affect both plot and table */}
      <div style={{ display: 'flex', alignItems: 'center', gap: 14, marginBottom: 14, flexWrap: 'wrap',
                    padding: '8px 12px', background: C.bgLight, border: `1px solid ${C.border}`, borderRadius: 5 }}>
        <div style={{ display: 'flex', alignItems: 'center', gap: 6 }}>
          <label style={{ fontSize: 12, color: C.textSec }}>Correlation cutoff |r| ≥</label>
          <input
            type="number" min="0" max="1" step="0.05"
            value={cutoffStr}
            onChange={e => setCutoffStr(e.target.value)}
            style={{ width: 64, padding: '3px 6px', fontSize: 12, border: `1px solid ${C.border}`, borderRadius: 4 }}
          />
          <ApplyBtn onClick={handleApply} busy={busy} label="Apply" />
          <span style={{ fontSize: 11, color: C.textMuted }}>current: {cutoff.toFixed(2)}</span>
        </div>
        <label style={{ fontSize: 12, color: C.textSec, display: 'flex', alignItems: 'center', gap: 6, cursor: 'pointer' }}>
          <input type="checkbox" checked={crossOnly} onChange={e => setCrossOnly(e.target.checked)} />
          Table: cross-block only
        </label>
        <RegenStatus busy={busy} error={error} />
      </div>

      {tab === 'network_plot' && (
        plotArt ? (
          <div>
            <PlotImg url={plotArt.png_url} alt="Network Plot" cacheBust={cacheBust} />
            <div style={{ display: 'flex', gap: 8, marginTop: 10 }}>
              <DownloadBtn label="PNG" url={plotArt.png_url} filename="network.png" />
              <DownloadBtn label="PDF" url={plotArt.pdf_url} filename="network.pdf" />
            </div>
            <div style={{ marginTop: 8, fontSize: 11, color: C.textMuted, fontStyle: 'italic' }}>
              Blue = positive correlation, Red = negative correlation. Node size = degree.
            </div>
          </div>
        ) : <Unavailable msg="Network plot was not generated for this run." />
      )}

      {tab === 'node_metrics' && (
        nodeArt ? (
          <div>
            <CsvTable url={nodeArt.csv_url}
              columns={[
                { key: 'feature',     label: 'Feature' },
                { key: 'block',       label: 'Block' },
                { key: 'degree',      label: 'Degree' },
                { key: 'betweenness', label: 'Betweenness', format: v => v === 'NA' || v === '' ? '—' : Number(v).toFixed(4) },
                { key: 'closeness',   label: 'Closeness',   format: v => v === 'NA' || v === '' ? '—' : Number(v).toFixed(4) },
                { key: 'rank',        label: 'Rank' },
              ]}
              pageSize={20}
            />
            <div style={{ marginTop: 8 }}>
              <DownloadBtn label="CSV" url={nodeArt.csv_url} filename="network_nodes.csv" />
            </div>
          </div>
        ) : <Unavailable msg="Node metrics table was not generated for this run." />
      )}

      {tab === 'edge_table' && (
        edgeArt ? (
          <CsvExpandableTable
            url={edgeArt.csv_url}
            downloadFilename="network_edges.csv"
            filterFn={crossOnly ? (r => r.within_block === 'FALSE' || r.within_block === false) : null}
            cutoff={cutoff}
          />
        ) : <Unavailable msg="Edge table was not generated for this run." />
      )}
    </div>
  );
}

// ─── Page: Performance ───────────────────────────────────────────────────────
function PerformancePage({ manifest, sessionId, onBack }) {
  const mEntries = manifest ? Object.values(manifest) : [];
  const base     = `/api/v1/${sessionId}/results/diablo`;

  const errArt = mEntries.find(a => a.tab_id === 'error_rate' && a.status === 'success') || null;
  const berArt = mEntries.find(a => a.tab_id === 'ber'        && a.status === 'success') || null;

  return (
    <div style={{ maxWidth: 860 }}>
      <BackBtn onClick={onBack} />
      <h2 style={{ fontSize: 17, fontWeight: 700, color: C.textPrimary, margin: '0 0 18px' }}>Performance</h2>

      {!errArt && !berArt && <Unavailable msg="Performance data was not generated for this run." />}

      {errArt && (
        <div style={{ marginBottom: 28 }}>
          <SectionTitle>Overall Error Rate (centroids.dist)</SectionTitle>
          <CsvTable url={errArt.csv_url}
            columns={[
              { key: 'component',  label: 'Component' },
              { key: 'error_pct',  label: 'Error Rate (%)' },
              { key: 'error_rate', label: 'Error Rate (raw)', format: v => isNaN(Number(v)) ? v : Number(v).toFixed(4) },
            ]}
            pageSize={20}
          />
          <div style={{ marginTop: 8 }}>
            <DownloadBtn label="CSV" url={errArt.csv_url} filename="performance_error_rate.csv" />
          </div>
        </div>
      )}

      {berArt && (
        <div style={{ marginBottom: 28 }}>
          <SectionTitle>Balanced Error Rate (BER) by Class</SectionTitle>
          <CsvTable url={berArt.csv_url}
            columns={[
              { key: 'component', label: 'Component' },
              { key: 'class',     label: 'Class' },
              { key: 'ber',       label: 'BER', format: v => isNaN(Number(v)) ? v : Number(v).toFixed(4) },
            ]}
            pageSize={20}
          />
          <div style={{ marginTop: 8 }}>
            <DownloadBtn label="CSV" url={berArt.csv_url} filename="performance_ber.csv" />
          </div>
        </div>
      )}

      <div style={{ marginTop: 8 }}>
        <DownloadBtn label="Performance JSON" url={`${base}/performance.json`} filename="performance.json" />
      </div>
    </div>
  );
}

// ─── Page: Export Results ─────────────────────────────────────────────────────
function ExportPage({ manifest, sessionId, onBack }) {
  const base     = `/api/v1/${sessionId}/results/diablo`;
  const mEntries = manifest ? Object.values(manifest) : [];

  const sections = [
    { label: 'Multi-Block sPLS-DA',    items: mEntries.filter(a => a.page === 'consensus'   && a.status === 'success') },
    { label: 'Block-Specific sPLS-DAs', items: mEntries.filter(a => a.page === 'components'  && a.status === 'success') },
    { label: 'Circos',                  items: mEntries.filter(a => a.page === 'circos'      && a.status === 'success') },
    { label: 'Network',                 items: mEntries.filter(a => a.page === 'network'     && a.status === 'success') },
    { label: 'Performance',             items: mEntries.filter(a => a.page === 'performance' && a.status === 'success') },
  ];

  return (
    <div style={{ maxWidth: 800 }}>
      <BackBtn onClick={onBack} />
      <h2 style={{ fontSize: 17, fontWeight: 700, color: C.textPrimary, margin: '0 0 18px' }}>Export Results</h2>

      <div style={{ marginBottom: 22 }}>
        <SectionTitle>Core Files</SectionTitle>
        <div style={{ display: 'flex', flexWrap: 'wrap', gap: 8 }}>
          <DownloadBtn label="Selected Features (CSV)" url={`${base}/selected_features.csv`}       filename="selected_features.csv" />
          <DownloadBtn label="Ranked Features (CSV)"   url={`${base}/ranked_features.csv`}         filename="ranked_features.csv" />
          <DownloadBtn label="Performance (JSON)"      url={`${base}/performance.json`}            filename="performance.json" />
          <DownloadBtn label="Results Summary (JSON)"  url={`${base}/diablo_results_summary.json`} filename="diablo_results_summary.json" />
          <DownloadBtn label="Results Manifest (JSON)" url={`${base}/results_manifest.json`}       filename="results_manifest.json" />
        </div>
      </div>

      {sections.map(sec => {
        if (sec.items.length === 0) return null;
        return (
          <div key={sec.label} style={{ marginBottom: 18 }}>
            <SectionTitle>{sec.label}</SectionTitle>
            <div style={{ display: 'flex', flexWrap: 'wrap', gap: 8 }}>
              {sec.items.map(art => (
                <span key={art.artifact_key} style={{ display: 'flex', gap: 4 }}>
                  {art.png_url && <DownloadBtn label={`${art.display_name} (PNG)`} url={art.png_url} filename={`${art.artifact_key}.png`} />}
                  {art.pdf_url && <DownloadBtn label={`${art.display_name} (PDF)`} url={art.pdf_url} filename={`${art.artifact_key}.pdf`} />}
                  {art.csv_url && <DownloadBtn label={`${art.display_name} (CSV)`} url={art.csv_url} filename={`${art.artifact_key}.csv`} />}
                </span>
              ))}
            </div>
          </div>
        );
      })}
    </div>
  );
}

// ─── Root component ───────────────────────────────────────────────────────────
export default function DiabloResultsStep({ sessionId, onRunAgain }) {
  const [summary,  setSummary]  = useState(null);
  const [manifest, setManifest] = useState(null);
  const [loading,  setLoading]  = useState(true);
  const [error,    setError]    = useState(null);
  const [page,     setPage]     = useState('hub');

  useEffect(() => {
    if (!sessionId) return;
    let cancelled = false;

    async function load() {
      try {
        const data = await apiGet(`/${sessionId}/analysis/diablo/results`);
        if (cancelled) return;
        if (data.status === 'error' || data.error_code) {
          setError(data.detail || 'Could not load DIABLO results.');
          return;
        }
        setSummary(data);

        try {
          const mRes = await fetch(`/api/v1/${sessionId}/results/diablo/results_manifest.json`);
          if (!cancelled && mRes.ok) setManifest(await mRes.json());
        } catch { /* manifest is optional */ }
      } catch (e) {
        if (!cancelled) setError(String(e.message ?? e));
      } finally {
        if (!cancelled) setLoading(false);
      }
    }

    load();
    return () => { cancelled = true; };
  }, [sessionId]);

  if (loading) return <p style={{ color: C.textMuted, fontSize: 13 }}>Loading DIABLO results…</p>;
  if (error)   return <p style={{ color: '#c0392b', fontSize: 13 }}>Error: {error}</p>;
  if (!summary) return null;

  const props = { summary, manifest, sessionId, onBack: () => setPage('hub') };

  if (page === 'hub')           return <HubPage          {...props} onNavigate={setPage} onRunAgain={onRunAgain} />;
  if (page === 'multiblock')    return <MultiBlockPage   {...props} />;
  if (page === 'blockspecific') return <BlockSpecificPage {...props} />;
  if (page === 'circos')        return <CircosPage       {...props} />;
  if (page === 'network')       return <NetworkPage      {...props} />;
  if (page === 'performance')   return <PerformancePage  {...props} />;
  if (page === 'export')        return <ExportPage       {...props} />;

  return null;
}
