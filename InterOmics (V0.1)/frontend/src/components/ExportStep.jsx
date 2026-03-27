// src/components/ExportStep.jsx — Export step
// Task 4.11
// Omissions are read from the X-Export-Omissions response header.

import React, { useState } from 'react';
import ErrorAlert from './ErrorAlert';

const API_BASE = '/api/v1';

const MODULES  = ['diablo', 'correlation', 'enrichment', 'stats'];
const FORMATS  = ['csv', 'png', 'pdf'];

export default function ExportStep({ sessionId }) {
  const [modules,   setModules]   = useState({ diablo: true, correlation: true, enrichment: true, stats: true });
  const [formats,   setFormats]   = useState({ csv: true, png: true, pdf: true });
  const [omissions, setOmissions] = useState(null);   // array of { path, reason }
  const [error,     setError]     = useState(null);
  const [loading,   setLoading]   = useState(false);

  function toggleModule(m) { setModules(prev => ({ ...prev, [m]: !prev[m] })); }
  function toggleFormat(f) { setFormats(prev => ({ ...prev, [f]: !prev[f] })); }

  async function handleExport(e) {
    e.preventDefault();
    setError(null);
    setOmissions(null);
    setLoading(true);

    const selectedModules = MODULES.filter(m => modules[m]);
    const selectedFormats = FORMATS.filter(f => formats[f]);

    if (selectedModules.length === 0 || selectedFormats.length === 0) {
      setError({ error_code: 'INVALID_PARAMS', detail: 'Select at least one module and one format.', recoverable: true });
      setLoading(false);
      return;
    }

    try {
      const res = await fetch(`${API_BASE}/${sessionId}/export`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ modules: selectedModules, formats: selectedFormats }),
      });

      if (!res.ok) {
        const errData = await res.json().catch(() => ({}));
        setError({ error_code: errData.error_code || 'EXPORT_FAILED', detail: errData.detail || `HTTP ${res.status}`, recoverable: true });
        return;
      }

      // Parse omissions from header before consuming body
      const omissionsHeader = res.headers.get('X-Export-Omissions');
      if (omissionsHeader) {
        try { setOmissions(JSON.parse(omissionsHeader)); } catch (_) { /* ignore parse errors */ }
      }

      // Trigger ZIP download
      const blob = await res.blob();
      const url  = URL.createObjectURL(blob);
      const a    = document.createElement('a');
      a.href     = url;
      a.download = `export_${sessionId}.zip`;
      document.body.appendChild(a);
      a.click();
      a.remove();
      URL.revokeObjectURL(url);
    } catch (err) {
      setError({ error_code: 'NETWORK_ERROR', detail: err.message, recoverable: true });
    } finally {
      setLoading(false);
    }
  }

  return (
    <div>
      <h2>Export Results</h2>
      <ErrorAlert error={error} onDismiss={() => setError(null)} />

      <form onSubmit={handleExport}>
        <fieldset style={fsStyle}>
          <legend>Modules</legend>
          <div style={{ display: 'flex', gap: 16, flexWrap: 'wrap' }}>
            {MODULES.map(m => (
              <label key={m} style={checkLabel}>
                <input type="checkbox" checked={modules[m]} onChange={() => toggleModule(m)} />
                {m.charAt(0).toUpperCase() + m.slice(1)}
              </label>
            ))}
          </div>
        </fieldset>

        <fieldset style={fsStyle}>
          <legend>Formats</legend>
          <div style={{ display: 'flex', gap: 16, flexWrap: 'wrap' }}>
            {FORMATS.map(f => (
              <label key={f} style={checkLabel}>
                <input type="checkbox" checked={formats[f]} onChange={() => toggleFormat(f)} />
                {f.toUpperCase()}
              </label>
            ))}
          </div>
        </fieldset>

        <button type="submit" disabled={loading} style={btnStyle(loading)}>
          {loading ? 'Preparing ZIP…' : 'Download ZIP'}
        </button>
      </form>

      {omissions && omissions.length > 0 && (
        <div style={{ marginTop: 20 }}>
          <h3 style={{ marginBottom: 8 }}>Omitted Files</h3>
          <p style={{ fontSize: 13, color: '#666', marginBottom: 8 }}>
            The following files were not included because they have not been generated yet:
          </p>
          <table style={tblStyle}>
            <thead>
              <tr>
                <th style={thStyle}>File Path</th>
                <th style={thStyle}>Reason</th>
              </tr>
            </thead>
            <tbody>
              {omissions.map((o, i) => (
                <tr key={i} style={{ background: i % 2 === 0 ? '#f9f9f9' : '#fff' }}>
                  <td style={tdStyle}><code style={{ fontSize: 12 }}>{o.path}</code></td>
                  <td style={tdStyle}>{o.reason}</td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      )}

      {omissions && omissions.length === 0 && (
        <p style={{ marginTop: 12, color: '#2a7a2a' }}>✓ All requested files were included in the export.</p>
      )}
    </div>
  );
}

const fsStyle   = { border: '1px solid #ddd', borderRadius: 4, padding: '10px 14px', marginBottom: 12 };
const checkLabel = { display: 'flex', alignItems: 'center', gap: 6, cursor: 'pointer', fontSize: 14 };
const tblStyle  = { borderCollapse: 'collapse', width: '100%', fontSize: 13 };
const thStyle   = { background: '#f0f0f0', padding: '6px 10px', textAlign: 'left', borderBottom: '2px solid #ccc' };
const tdStyle   = { padding: '5px 10px', borderBottom: '1px solid #eee' };
const btnStyle  = loading => ({
  padding: '8px 20px',
  background: loading ? '#aaa' : '#0066cc',
  color: '#fff',
  border: 'none',
  borderRadius: 4,
  cursor: loading ? 'not-allowed' : 'pointer',
});
