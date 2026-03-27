// src/components/ValidationStep.jsx — DIABLO Validate Inputs step

import { useEffect, useState } from 'react';
import { apiPost } from '../api/session';
import ErrorAlert from './ErrorAlert';

const C = {
  maroon:     '#7b1c2e',
  maroonLight:'#f5eaec',
  border:     '#e2e2e5',
  textPrimary:'#1a1a24',
  textSec:    '#555563',
  textMuted:  '#888896',
  bg:         '#f4f4f5',
  green:      '#2d6a4f',
  greenLight: '#d8f3dc',
  warnBg:     '#fff3cd',
  warnBorder: '#ffc107',
  warnText:   '#856404',
};

const CHECKS = [
  { id: 'sampleid',    label: 'SampleID column present in all files' },
  { id: 'consistency', label: 'SampleIDs consistent across all omics blocks' },
  { id: 'metadata',    label: 'All matrix SampleIDs found in metadata' },
  { id: 'numeric',     label: 'All feature columns are numeric' },
  { id: 'duplicates',  label: 'No duplicate SampleIDs (blank rows removed first)' },
  { id: 'missing',     label: 'Missing-value summary computed per block' },
];

export default function ValidationStep({ sessionId, onComplete }) {
  const [report,  setReport]  = useState(null);
  const [error,   setError]   = useState(null);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    async function run() {
      try {
        const data = await apiPost(`/${sessionId}/validate`, {});
        if (data.status === 'error') setError(data);
        else setReport(data);
      } catch (err) {
        setError({ error_code: 'NETWORK_ERROR', detail: String(err.message ?? err), recoverable: true });
      } finally {
        setLoading(false);
      }
    }
    run();
  }, [sessionId]);

  const hasCriticalError = !!error;
  const warnings = report?.warnings ?? [];

  return (
    <div>
      <h2 style={headingStyle}>Validate Inputs</h2>

      <div style={infoBoxStyle}>
        <p style={infoLabelStyle}>What DIABLO validation checks</p>
        <ul style={{ margin: 0, paddingLeft: 18 }}>
          {CHECKS.map(c => (
            <li key={c.id} style={{ fontSize: 13, color: C.textSec, marginBottom: 3 }}>{c.label}</li>
          ))}
        </ul>
        <p style={{ fontSize: 12, color: C.textMuted, margin: '8px 0 0' }}>
          Metadata may contain extra rows beyond the uploaded matrices. All matrix SampleIDs must exist in metadata after blank-row removal.
        </p>
      </div>

      {loading && <div style={{ color: C.textSec, fontSize: 14, padding: '12px 0' }}>Running validation…</div>}

      <ErrorAlert error={error} onDismiss={() => setError(null)} />

      {hasCriticalError && (
        <p style={{ color: '#c0392b', fontSize: 13, marginTop: 4 }}>
          Correct the errors above and re-upload your files before continuing.
        </p>
      )}

      {report && (
        <div>
          {warnings.length > 0 && (
            <div style={{ marginBottom: 14 }}>
              {warnings.map((w, i) => (
                <div key={i} role="alert" style={warnStyle}>
                  <strong>{w.type}</strong>: {w.detail}
                </div>
              ))}
            </div>
          )}

          {!hasCriticalError && warnings.length === 0 && (
            <div style={successStyle}>Validation passed — no errors or warnings.</div>
          )}
          {!hasCriticalError && warnings.length > 0 && (
            <div style={successStyle}>Validation passed with {warnings.length} warning{warnings.length > 1 ? 's' : ''}.</div>
          )}

          {report.missing_value_pct && Object.keys(report.missing_value_pct).length > 0 && (
            <div style={{ marginBottom: 16 }}>
              <p style={sectionLabelStyle}>Missing-value summary</p>
              <table style={tableStyle}>
                <thead>
                  <tr>
                    {['Dataset', 'Samples', 'Features', 'Features with missing', 'Overall missing %'].map(h => (
                      <th key={h} style={thStyle}>{h}</th>
                    ))}
                  </tr>
                </thead>
                <tbody>
                  {Object.entries(report.missing_value_pct).map(([name, s]) => (
                    <tr key={name}>
                      <td style={tdStyle}>{name}</td>
                      <td style={tdStyle}>{s.n_samples ?? '—'}</td>
                      <td style={tdStyle}>{s.n_features_total ?? '—'}</td>
                      <td style={tdStyle}>{s.n_features_with_mv ?? '—'}</td>
                      <td style={tdStyle}>
                        {s.overall_pct != null && !isNaN(s.overall_pct)
                          ? `${Number(s.overall_pct).toFixed(1)}%`
                          : '0.0%'}
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}

          {report.sample_counts_per_group && Object.keys(report.sample_counts_per_group).length > 0 && (
            <div style={{ marginBottom: 16 }}>
              <p style={sectionLabelStyle}>Sample counts by group (first metadata column)</p>
              {Object.entries(report.sample_counts_per_group).map(([col, counts]) => (
                <div key={col} style={{ marginBottom: 6 }}>
                  <span style={{ fontSize: 12, color: C.textMuted, marginRight: 8 }}>{col}:</span>
                  {Object.entries(counts).map(([grp, n]) => (
                    <span key={grp} style={groupBadgeStyle}>{grp}: {n}</span>
                  ))}
                </div>
              ))}
            </div>
          )}

          <button onClick={onComplete} disabled={hasCriticalError} style={btnStyle(hasCriticalError)}>
            Continue to Define Comparison
          </button>
        </div>
      )}
    </div>
  );
}

const headingStyle      = { fontSize: 20, fontWeight: 700, color: C.textPrimary, margin: '0 0 14px' };
const infoBoxStyle      = { background: C.maroonLight, border: '1px solid #e2c8cc', borderRadius: 6, padding: '10px 14px', marginBottom: 18, maxWidth: 640, textAlign: 'left' };
const infoLabelStyle    = { fontSize: 12, fontWeight: 700, color: C.maroon, margin: '0 0 6px', textTransform: 'uppercase', letterSpacing: '0.06em' };
const sectionLabelStyle = { fontSize: 13, fontWeight: 700, color: C.textPrimary, margin: '0 0 6px' };
const warnStyle         = { background: C.warnBg, border: `1px solid ${C.warnBorder}`, borderRadius: 4, padding: '8px 12px', marginBottom: 6, color: C.warnText, fontSize: 13 };
const successStyle      = { background: C.greenLight, border: '1px solid #b7e4c7', borderRadius: 4, padding: '8px 12px', marginBottom: 14, color: C.green, fontSize: 13, maxWidth: 640 };
const tableStyle        = { width: '100%', borderCollapse: 'collapse', fontSize: 13 };
const thStyle           = { padding: '5px 10px', background: C.bg, border: `1px solid ${C.border}`, textAlign: 'left', fontWeight: 600, fontSize: 12 };
const tdStyle           = { padding: '5px 10px', border: `1px solid ${C.border}`, color: C.textSec };
const groupBadgeStyle   = { display: 'inline-block', background: C.bg, border: `1px solid ${C.border}`, borderRadius: 3, padding: '1px 7px', fontSize: 12, color: C.textSec, marginRight: 4 };
const btnStyle = disabled => ({
  padding: '9px 22px',
  background: disabled ? '#bbb' : C.maroon,
  color: '#fff',
  border: 'none',
  borderRadius: 5,
  cursor: disabled ? 'not-allowed' : 'pointer',
  fontWeight: 600,
  fontSize: 14,
});
