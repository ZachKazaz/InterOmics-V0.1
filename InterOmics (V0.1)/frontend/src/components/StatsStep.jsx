// src/components/StatsStep.jsx — Statistical modeling step
// Task 4.10

import React, { useState } from 'react';
import { apiPost } from '../api/session';
import ErrorAlert from './ErrorAlert';
import EducationPanel from './EducationPanel';
import Breadcrumb from './Breadcrumb';
import HelpTooltip from './HelpTooltip';

const EDUCATION = {
  what_it_does: 'Fits a linear model (or mixed-effects model for repeated measures) per feature, extracting standardized betas, partial eta-squared, and FDR-adjusted p-values.',
  when_to_use: 'After preprocessing. Use when you want to test associations between omics features and sample metadata variables.',
  input_requirements: 'Preprocessed feature matrices and a metadata file with at least one predictor column.',
  output_interpretation: 'Feature results table ranked by FDR. Standardized beta indicates effect direction and magnitude; partial eta-squared indicates variance explained.',
  common_mistakes: [
    'Including a covariate that is collinear with the predictor will inflate standard errors.',
    'Repeated-measures ID must uniquely identify subjects, not observations.',
  ],
};

export default function StatsStep({ sessionId, metadataColumns, onGoHome }) {
  const cols = metadataColumns || [];

  const [predictors,   setPredictors]   = useState([]);
  const [covariates,   setCovariates]   = useState([]);
  const [interactions, setInteractions] = useState([]);   // array of [colA, colB]
  const [rmId,         setRmId]         = useState('');
  const [intA,         setIntA]         = useState(cols[0] || '');
  const [intB,         setIntB]         = useState(cols[1] || '');

  const [result,  setResult]  = useState(null);
  const [error,   setError]   = useState(null);
  const [loading, setLoading] = useState(false);

  function toggleCol(col, list, setList) {
    setList(prev =>
      prev.includes(col) ? prev.filter(c => c !== col) : [...prev, col]
    );
  }

  function addInteraction() {
    if (!intA || !intB || intA === intB) return;
    const pair = [intA, intB].sort().join(':');
    if (!interactions.find(p => p.join(':') === pair)) {
      setInteractions(prev => [...prev, [intA, intB]]);
    }
  }

  function removeInteraction(idx) {
    setInteractions(prev => prev.filter((_, i) => i !== idx));
  }

  async function handleSubmit(e) {
    e.preventDefault();
    if (predictors.length === 0) return;
    setError(null);
    setResult(null);
    setLoading(true);
    try {
      const body = {
        predictors,
        covariates,
        interactions: interactions.map(p => p.join(':')),
        repeated_measures_id: rmId || null,
      };
      const data = await apiPost(`/${sessionId}/analysis/stats`, body);
      const payload = data.data || data;
      if (payload.status === 'error') setError(payload);
      else setResult(payload);
    } catch (err) {
      setError({ error_code: 'NETWORK_ERROR', detail: err.message, recoverable: true });
    } finally {
      setLoading(false);
    }
  }

  return (
    <div>
      <Breadcrumb crumbs={[
        { label: 'Home', onClick: onGoHome },
        { label: 'Statistical Modeling' },
      ]} />
      <h2 style={headingStyle}>Statistical Modeling</h2>

      <OutputsBox outputs={[
        'Per-feature results table: standardized beta, partial η², p-value, FDR',
        'Main effects and interaction effects summary tables',
        'Volcano and effect size plots',
      ]} />

      <EducationPanel education={EDUCATION} />
      <ErrorAlert error={error} onDismiss={() => setError(null)} />

      <form onSubmit={handleSubmit}>
        <fieldset style={fsStyle}>
          <legend style={{ display: 'flex', alignItems: 'center', gap: 4 }}>
            Predictors (required)
            <HelpTooltip text="Metadata columns to test as main effects. At least one is required." />
          </legend>
          <MultiSelect cols={cols} selected={predictors} onToggle={col => toggleCol(col, predictors, setPredictors)} />
          {predictors.length === 0 && cols.length > 0 && (
            <p style={{ fontSize: 12, color: '#c0392b', margin: '4px 0 0' }}>Select at least one predictor.</p>
          )}
        </fieldset>

        <fieldset style={fsStyle}>
          <legend style={{ display: 'flex', alignItems: 'center', gap: 4 }}>
            Covariates (optional)
            <HelpTooltip text="Metadata columns to include as covariates (controlled for but not the focus of inference)." />
          </legend>
          <MultiSelect cols={cols} selected={covariates} onToggle={col => toggleCol(col, covariates, setCovariates)} />
        </fieldset>

        <fieldset style={fsStyle}>
          <legend style={{ display: 'flex', alignItems: 'center', gap: 4 }}>
            Interaction Terms (optional)
            <HelpTooltip text="Test whether the effect of one variable depends on another. Select two columns and click Add." />
          </legend>
          <div style={{ display: 'flex', gap: 8, alignItems: 'center', marginBottom: 8 }}>
            <select value={intA} onChange={e => setIntA(e.target.value)} style={selStyle}>
              {cols.map(c => <option key={c} value={c}>{c}</option>)}
            </select>
            <span>×</span>
            <select value={intB} onChange={e => setIntB(e.target.value)} style={selStyle}>
              {cols.map(c => <option key={c} value={c}>{c}</option>)}
            </select>
            <button type="button" onClick={addInteraction} style={smallBtnStyle}>Add</button>
          </div>
          {interactions.length > 0 && (
            <ul style={{ margin: 0, paddingLeft: 16 }}>
              {interactions.map((pair, i) => (
                <li key={i} style={{ marginBottom: 4 }}>
                  {pair.join(' × ')}
                  <button type="button" onClick={() => removeInteraction(i)}
                    style={{ marginLeft: 8, background: 'none', border: 'none', color: '#c00', cursor: 'pointer' }}>
                    ✕
                  </button>
                </li>
              ))}
            </ul>
          )}
        </fieldset>

        <fieldset style={fsStyle}>
          <legend style={{ display: 'flex', alignItems: 'center', gap: 4 }}>
            Repeated-Measures Subject ID (optional)
            <HelpTooltip text="If samples are repeated measures from the same subject, select the column that uniquely identifies each subject." />
          </legend>
          <select value={rmId} onChange={e => setRmId(e.target.value)} style={selStyle}>
            <option value="">None</option>
            {cols.map(c => <option key={c} value={c}>{c}</option>)}
          </select>
        </fieldset>

        <button type="submit" disabled={loading || predictors.length === 0} style={btnStyle(loading || predictors.length === 0)}>
          {loading ? 'Running…' : 'Run Statistical Modeling'}
        </button>
      </form>

      {result && <StatsResults result={result} sessionId={sessionId} />}
    </div>
  );
}

// ---------------------------------------------------------------------------
// Results display
// ---------------------------------------------------------------------------
function StatsResults({ result }) {
  const [page, setPage] = useState(0);
  const PAGE_SIZE = 25;
  const rows = result.feature_results || [];
  const totalPages = Math.ceil(rows.length / PAGE_SIZE);
  const pageRows = rows.slice(page * PAGE_SIZE, (page + 1) * PAGE_SIZE);

  return (
    <div style={{ marginTop: 20 }}>
      {result.plots && result.plots.length > 0 && (
        <div style={{ display: 'flex', flexWrap: 'wrap', gap: 12, marginBottom: 16 }}>
          {result.plots.map((url, i) => (
            <img key={i} src={url} alt={`stats-plot-${i}`} style={{ maxWidth: 400, border: '1px solid #ddd' }} />
          ))}
        </div>
      )}

      {rows.length > 0 && (
        <>
          <h3 style={{ marginBottom: 8 }}>Feature Results</h3>
          <div style={{ overflowX: 'auto' }}>
            <table style={tblStyle}>
              <thead>
                <tr>
                  {['Feature', 'Term', 'Std Beta', 'Partial η²', 'p-value', 'FDR'].map(h => (
                    <th key={h} style={thStyle}>{h}</th>
                  ))}
                </tr>
              </thead>
              <tbody>
                {pageRows.map((row, i) => (
                  <tr key={i} style={{ background: i % 2 === 0 ? '#f9f9f9' : '#fff' }}>
                    <td style={tdStyle}>{row.feature}</td>
                    <td style={tdStyle}>{row.term}</td>
                    <td style={tdStyle}>{fmtNum(row.std_beta)}</td>
                    <td style={tdStyle}>{fmtNum(row.partial_eta_sq)}</td>
                    <td style={tdStyle}>{fmtNum(row.p_value)}</td>
                    <td style={tdStyle}>{fmtNum(row.fdr)}</td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
          {totalPages > 1 && (
            <div style={{ marginTop: 8, display: 'flex', gap: 8, alignItems: 'center' }}>
              <button onClick={() => setPage(p => Math.max(0, p - 1))} disabled={page === 0} style={smallBtnStyle}>← Prev</button>
              <span>Page {page + 1} / {totalPages}</span>
              <button onClick={() => setPage(p => Math.min(totalPages - 1, p + 1))} disabled={page === totalPages - 1} style={smallBtnStyle}>Next →</button>
            </div>
          )}
        </>
      )}

      {result.main_effects_summary && result.main_effects_summary.length > 0 && (
        <SummaryTable title="Main Effects Summary" rows={result.main_effects_summary} />
      )}
      {result.interaction_effects_summary && result.interaction_effects_summary.length > 0 && (
        <SummaryTable title="Interaction Effects Summary" rows={result.interaction_effects_summary} />
      )}
    </div>
  );
}

function SummaryTable({ title, rows }) {
  if (!rows || rows.length === 0) return null;
  const cols = Object.keys(rows[0]);
  return (
    <div style={{ marginTop: 16 }}>
      <h3 style={{ marginBottom: 8 }}>{title}</h3>
      <div style={{ overflowX: 'auto' }}>
        <table style={tblStyle}>
          <thead>
            <tr>{cols.map(c => <th key={c} style={thStyle}>{c}</th>)}</tr>
          </thead>
          <tbody>
            {rows.map((row, i) => (
              <tr key={i} style={{ background: i % 2 === 0 ? '#f9f9f9' : '#fff' }}>
                {cols.map(c => <td key={c} style={tdStyle}>{fmtNum(row[c])}</td>)}
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </div>
  );
}

// ---------------------------------------------------------------------------
// Multi-select checkbox list
// ---------------------------------------------------------------------------
function MultiSelect({ cols, selected, onToggle }) {
  if (cols.length === 0) return <p style={{ color: '#888', fontSize: 13 }}>No metadata columns available.</p>;
  return (
    <div style={{ display: 'flex', flexWrap: 'wrap', gap: '4px 16px' }}>
      {cols.map(col => (
        <label key={col} style={{ display: 'flex', alignItems: 'center', gap: 4, cursor: 'pointer', fontSize: 13 }}>
          <input
            type="checkbox"
            checked={selected.includes(col)}
            onChange={() => onToggle(col)}
          />
          {col}
        </label>
      ))}
    </div>
  );
}

// ---------------------------------------------------------------------------
// Shared helpers
// ---------------------------------------------------------------------------
function fmtNum(n) {
  if (n == null) return '—';
  return typeof n === 'number' ? n.toExponential(3) : n;
}

const headingStyle = { fontSize: 20, fontWeight: 700, color: '#1a1a24', marginBottom: 16, marginTop: 0 };
const fsStyle      = { border: '1px solid #ddd', borderRadius: 4, padding: '10px 14px', marginBottom: 12 };
const selStyle     = { padding: '4px 6px' };
const tblStyle     = { borderCollapse: 'collapse', width: '100%', fontSize: 13 };
const thStyle      = { background: '#f0f0f0', padding: '6px 10px', textAlign: 'left', borderBottom: '2px solid #ccc' };
const tdStyle      = { padding: '5px 10px', borderBottom: '1px solid #eee' };
const btnStyle     = disabled => ({
  padding: '9px 22px',
  background: disabled ? '#aaa' : '#7b1c2e',
  color: '#fff',
  border: 'none',
  borderRadius: 5,
  cursor: disabled ? 'not-allowed' : 'pointer',
  fontWeight: 600,
  fontSize: 14,
});
const smallBtnStyle = {
  padding: '4px 10px',
  background: '#e9ecef',
  border: '1px solid #ccc',
  borderRadius: 4,
  cursor: 'pointer',
  fontSize: 13,
};

function OutputsBox({ outputs }) {
  return (
    <div style={{ background: '#f5eaec', border: '1px solid #e2c8cc', borderRadius: 6, padding: '10px 14px', marginBottom: 18 }}>
      <p style={{ fontSize: 12, fontWeight: 700, color: '#7b1c2e', margin: '0 0 6px', textTransform: 'uppercase', letterSpacing: '0.06em' }}>
        Outputs you will get
      </p>
      <ul style={{ margin: 0, paddingLeft: 18 }}>
        {outputs.map((o, i) => (
          <li key={i} style={{ fontSize: 13, color: '#555563', marginBottom: 2 }}>{o}</li>
        ))}
      </ul>
    </div>
  );
}
