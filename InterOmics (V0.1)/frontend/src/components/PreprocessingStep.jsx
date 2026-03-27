// src/components/PreprocessingStep.jsx — DIABLO Preprocess Data step

import React, { useState } from 'react';
import { apiPost } from '../api/session';
import ErrorAlert from './ErrorAlert';

const C = {
  maroon:     '#7b1c2e',
  border:     '#e2e2e5',
  textPrimary:'#1a1a24',
  textSec:    '#555563',
  textMuted:  '#888896',
  bg:         '#f4f4f5',
  green:      '#2d6a4f',
  greenLight: '#d8f3dc',
};

const SUBSTEPS = [
  { id: 'filter',    label: 'Filtering' },
  { id: 'impute',    label: 'Imputation' },
  { id: 'normalize', label: 'Normalization' },
  { id: 'transform', label: 'Transformation' },
  { id: 'scale',     label: 'Scaling' },
];

const MISSINGNESS_OPTS = ['0.2', '0.3', '0.5', '0.8'];
const VARIANCE_OPTS    = ['none', 'IQR', 'SD', 'MAD'];
const IMPUTE_OPTS      = ['none', 'half_min', 'mean', 'median', 'knn'];
const NORM_OPTS        = ['none', 'total_sum', 'median', 'pqn', 'reference_sample'];
const TRANSFORM_OPTS   = ['none', 'log2', 'log10', 'sqrt', 'cuberoot'];
const SCALE_OPTS       = ['none', 'mean_center', 'auto', 'pareto', 'range'];

const HELP = {
  missingness: 'Features missing in more than this fraction of samples are removed.',
  variance:    'Remove low-variance features. IQR, SD, or MAD; "none" skips this filter.',
  abundance:   'Features with median below this value are removed. Leave blank to skip.',
  impute:      'Replace missing values. half_min uses half the per-feature minimum.',
  normalize:   'Correct sample-level technical variation (e.g. total sum, PQN).',
  transform:   'Reduce skewness. log2 is standard for metabolomics.',
  scale:       'Adjust feature variance. Pareto scaling is common for metabolomics.',
};

export default function PreprocessingStep({ sessionId, onComplete }) {
  const [activeSubstep, setActiveSubstep] = useState('filter');
  const [params, setParams] = useState({
    filtering:      { missingness_threshold: '0.5', variance_method: 'none', low_abundance_min: '' },
    imputation:     { method: 'none' },
    normalization:  { method: 'none' },
    transformation: { method: 'none' },
    scaling:        { method: 'none' },
  });
  const [summary, setSummary] = useState(null);
  const [error,   setError]   = useState(null);
  const [loading, setLoading] = useState(false);

  function set(section, key, value) {
    setParams(p => ({ ...p, [section]: { ...p[section], [key]: value } }));
  }

  async function handleRun() {
    setError(null);
    setSummary(null);
    setLoading(true);

    const body = {
      filtering: {
        missingness_threshold: parseFloat(params.filtering.missingness_threshold),
        variance_method: params.filtering.variance_method === 'none' ? null : params.filtering.variance_method,
        low_abundance_min: params.filtering.low_abundance_min !== '' ? parseFloat(params.filtering.low_abundance_min) : null,
      },
      imputation:     { method: params.imputation.method },
      normalization:  { method: params.normalization.method },
      transformation: { method: params.transformation.method },
      scaling:        { method: params.scaling.method },
    };

    try {
      const data = await apiPost(`/${sessionId}/preprocess`, body);
      if (data.status === 'error') {
        setError(data);
      } else {
        setSummary(data.summary || data.data?.summary || {});
      }
    } catch (err) {
      setError({ error_code: 'NETWORK_ERROR', detail: err.message, recoverable: true });
    } finally {
      setLoading(false);
    }
  }

  return (
    <div>
      <h2 style={headingStyle}>Preprocess Data</h2>
      <p style={{ fontSize: 13, color: C.textSec, margin: '0 0 14px' }}>
        The same preprocessing settings are applied consistently across all omics blocks.
      </p>

      {/* Horizontal preprocessing substep row — inside Preprocess page only */}
      <div style={substepNavStyle}>
        {SUBSTEPS.map((s, i) => (
          <React.Fragment key={s.id}>
            <button onClick={() => setActiveSubstep(s.id)} style={substepBtnStyle(s.id === activeSubstep)}>
              {s.label}
            </button>
            {i < SUBSTEPS.length - 1 && <span style={chevronStyle}>›</span>}
          </React.Fragment>
        ))}
      </div>

      <ErrorAlert error={error} onDismiss={() => setError(null)} />

      <div style={panelStyle}>
        {activeSubstep === 'filter' && (
          <div>
            <p style={panelDescStyle}>Remove low-quality features before analysis.</p>
            <FormRow label="Missingness threshold" help={HELP.missingness}>
              <select value={params.filtering.missingness_threshold}
                onChange={e => set('filtering', 'missingness_threshold', e.target.value)} style={selStyle}>
                {MISSINGNESS_OPTS.map(o => <option key={o} value={o}>{o}</option>)}
              </select>
            </FormRow>
            <FormRow label="Variance filter" help={HELP.variance}>
              <select value={params.filtering.variance_method}
                onChange={e => set('filtering', 'variance_method', e.target.value)} style={selStyle}>
                {VARIANCE_OPTS.map(o => <option key={o} value={o}>{o}</option>)}
              </select>
            </FormRow>
            <FormRow label="Low abundance min" help={HELP.abundance}>
              <input type="number" step="any" placeholder="none"
                value={params.filtering.low_abundance_min}
                onChange={e => set('filtering', 'low_abundance_min', e.target.value)}
                style={{ ...selStyle, width: 90 }} />
            </FormRow>
          </div>
        )}
        {activeSubstep === 'impute' && (
          <div>
            <p style={panelDescStyle}>Replace missing values before normalization.</p>
            <FormRow label="Method" help={HELP.impute}>
              <select value={params.imputation.method}
                onChange={e => set('imputation', 'method', e.target.value)} style={selStyle}>
                {IMPUTE_OPTS.map(o => <option key={o} value={o}>{o}</option>)}
              </select>
            </FormRow>
          </div>
        )}
        {activeSubstep === 'normalize' && (
          <div>
            <p style={panelDescStyle}>Correct for sample-level technical variation.</p>
            <FormRow label="Method" help={HELP.normalize}>
              <select value={params.normalization.method}
                onChange={e => set('normalization', 'method', e.target.value)} style={selStyle}>
                {NORM_OPTS.map(o => <option key={o} value={o}>{o}</option>)}
              </select>
            </FormRow>
          </div>
        )}
        {activeSubstep === 'transform' && (
          <div>
            <p style={panelDescStyle}>Reduce distributional skewness.</p>
            <FormRow label="Method" help={HELP.transform}>
              <select value={params.transformation.method}
                onChange={e => set('transformation', 'method', e.target.value)} style={selStyle}>
                {TRANSFORM_OPTS.map(o => <option key={o} value={o}>{o}</option>)}
              </select>
            </FormRow>
          </div>
        )}
        {activeSubstep === 'scale' && (
          <div>
            <p style={panelDescStyle}>Adjust feature variance for comparability across blocks.</p>
            <FormRow label="Method" help={HELP.scale}>
              <select value={params.scaling.method}
                onChange={e => set('scaling', 'method', e.target.value)} style={selStyle}>
                {SCALE_OPTS.map(o => <option key={o} value={o}>{o}</option>)}
              </select>
            </FormRow>
          </div>
        )}
      </div>

      <button onClick={handleRun} disabled={loading} style={btnStyle(loading)}>
        {loading ? 'Processing…' : 'Run Preprocessing'}
      </button>

      {summary && (
        <div style={summaryStyle}>
          <p style={{ margin: '0 0 6px', fontWeight: 600, color: C.green, fontSize: 13 }}>Preprocessing complete.</p>
          <table style={{ borderCollapse: 'collapse', fontSize: 13, width: '100%' }}>
            <tbody>
              {[
                ['Steps applied',     (summary.steps_applied || []).join(', ') || '—'],
                ['Features removed',  summary.features_removed ?? '—'],
                ['Features retained', summary.features_retained ?? '—'],
                ['Values imputed',    summary.values_imputed ?? '—'],
              ].map(([k, v]) => (
                <tr key={k}>
                  <td style={{ padding: '3px 10px 3px 0', color: C.textMuted, width: 160 }}>{k}</td>
                  <td style={{ padding: '3px 0', color: C.textPrimary }}>{v}</td>
                </tr>
              ))}
            </tbody>
          </table>
          <button onClick={onComplete} style={{ ...btnStyle(false), marginTop: 14 }}>
            Continue to Configure DIABLO
          </button>
        </div>
      )}
    </div>
  );
}

function FormRow({ label, help, children }) {
  const [showHelp, setShowHelp] = useState(false);
  return (
    <div style={{ display: 'flex', alignItems: 'center', marginBottom: 10, gap: 8 }}>
      <span style={{ fontSize: 13, color: '#1a1a24', minWidth: 180 }}>{label}</span>
      {children}
      <button type="button" onClick={() => setShowHelp(h => !h)} title="Help" style={helpBtnStyle}>?</button>
      {showHelp && <span style={{ fontSize: 12, color: '#555563', marginLeft: 4 }}>{help}</span>}
    </div>
  );
}

const headingStyle    = { fontSize: 20, fontWeight: 700, color: '#1a1a24', margin: '0 0 6px' };
const substepNavStyle = { display: 'flex', alignItems: 'center', gap: 2, marginBottom: 14 };
const chevronStyle    = { color: '#aaaabc', fontSize: 14, userSelect: 'none' };
const panelStyle      = { border: '1px solid #e2e2e5', borderRadius: 6, padding: '14px 16px', marginBottom: 16, background: '#fff', maxWidth: 560 };
const panelDescStyle  = { fontSize: 13, color: '#555563', margin: '0 0 12px' };
const selStyle        = { padding: '4px 8px', fontSize: 13, borderRadius: 4, border: '1px solid #ccc' };
const summaryStyle    = { marginTop: 16, padding: '12px 16px', background: '#d8f3dc', border: '1px solid #b7e4c7', borderRadius: 6, maxWidth: 560 };
const helpBtnStyle    = { width: 18, height: 18, borderRadius: '50%', border: '1px solid #ccc', background: '#f4f4f5', cursor: 'pointer', fontSize: 11, color: '#555563', padding: 0, lineHeight: '16px', flexShrink: 0 };

const substepBtnStyle = active => ({
  padding: '5px 12px',
  fontSize: 12,
  fontWeight: active ? 700 : 400,
  background: active ? '#7b1c2e' : '#fff',
  color: active ? '#fff' : '#555563',
  border: '1px solid',
  borderColor: active ? '#7b1c2e' : '#e2e2e5',
  borderRadius: 4,
  cursor: 'pointer',
});

const btnStyle = disabled => ({
  padding: '9px 22px',
  background: disabled ? '#bbb' : '#7b1c2e',
  color: '#fff',
  border: 'none',
  borderRadius: 5,
  cursor: disabled ? 'not-allowed' : 'pointer',
  fontWeight: 600,
  fontSize: 14,
});
