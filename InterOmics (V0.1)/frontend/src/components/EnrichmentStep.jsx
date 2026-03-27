// src/components/EnrichmentStep.jsx — Enrichment analysis step
// Task 4.9

import React, { useState } from 'react';
import { apiPost, apiGet } from '../api/session';
import ErrorAlert from './ErrorAlert';
import EducationPanel from './EducationPanel';
import Breadcrumb from './Breadcrumb';

const EDUCATION = {
  what_it_does: 'Tests whether your mapped features are statistically over-represented in known biological pathways (ORA), accounts for pathway topology (Topology), or identifies enriched lipid classes.',
  when_to_use: 'After ID mapping. ORA gives a quick pathway overview; Topology adds mechanistic context; Lipid Class is specific to lipidomics datasets.',
  input_requirements: 'Completed ID mapping step. ORA and Topology require KEGG IDs; Lipid Class requires lipid class assignments.',
  output_interpretation: 'Enriched pathways ranked by FDR-adjusted p-value. Impact score (Topology) reflects how central your features are within the pathway graph.',
  common_mistakes: [
    'Running Topology before ORA — ORA results are required as input.',
    'Low mapping success rate reduces statistical power for ORA.',
  ],
};

const TABS = ['ora', 'topology', 'lipid'];
const TAB_LABELS = { ora: 'ORA', topology: 'Pathway Topology', lipid: 'Lipid Class Enrichment' };

export default function EnrichmentStep({ sessionId, onGoHome }) {
  const [tab, setTab] = useState('ora');

  return (
    <div>
      <Breadcrumb crumbs={[
        { label: 'Home', onClick: onGoHome },
        { label: 'Enrichment Analysis' },
      ]} />
      <h2 style={headingStyle}>Enrichment Analysis</h2>

      <OutputsBox outputs={[
        'Enriched pathways ranked by FDR-adjusted p-value',
        'Pathway diagrams with matched compounds highlighted (ORA)',
        'Impact scores reflecting topological centrality (Topology)',
        'Enriched lipid classes with matched and background counts',
      ]} />

      <EducationPanel education={EDUCATION} />

      <div style={{ display: 'flex', gap: 0, marginBottom: 16 }}>
        {TABS.map((t, i) => (
          <button
            key={t}
            onClick={() => setTab(t)}
            style={{
              padding: '8px 20px',
              background: tab === t ? '#7b1c2e' : '#f4f4f5',
              color: tab === t ? '#fff' : '#1a1a24',
              border: '1px solid #e2e2e5',
              borderLeft: i > 0 ? 'none' : '1px solid #e2e2e5',
              cursor: 'pointer',
              fontWeight: tab === t ? 700 : 400,
              fontSize: 13,
            }}
          >
            {TAB_LABELS[t]}
          </button>
        ))}
      </div>

      {tab === 'ora'      && <ORATab      sessionId={sessionId} />}
      {tab === 'topology' && <TopologyTab sessionId={sessionId} />}
      {tab === 'lipid'    && <LipidTab    sessionId={sessionId} />}
    </div>
  );
}

// ---------------------------------------------------------------------------
// ORA tab
// ---------------------------------------------------------------------------
function ORATab({ sessionId }) {
  const [database, setDatabase] = useState('KEGG');
  const [result, setResult]     = useState(null);
  const [error, setError]       = useState(null);
  const [loading, setLoading]   = useState(false);
  const [pathwayImage, setPathwayImage]   = useState(null);   // { url, note, warning }
  const [pathwayLoading, setPathwayLoading] = useState(false);
  const [activePathway, setActivePathway]   = useState(null);

  async function handleSubmit(e) {
    e.preventDefault();
    setError(null);
    setResult(null);
    setPathwayImage(null);
    setActivePathway(null);
    setLoading(true);
    try {
      const data = await apiPost(`/${sessionId}/analysis/enrichment/ora`, { database });
      const payload = data.data || data;
      if (payload.status === 'error') setError(payload);
      else setResult(payload);
    } catch (err) {
      setError({ error_code: 'NETWORK_ERROR', detail: err.message, recoverable: true });
    } finally {
      setLoading(false);
    }
  }

  async function viewPathway(pathwayId) {
    setActivePathway(pathwayId);
    setPathwayImage(null);
    setPathwayLoading(true);
    try {
      const data = await apiGet(`/${sessionId}/analysis/enrichment/ora/pathway/${pathwayId}`);
      const payload = data.data || data;
      setPathwayImage(payload);
    } catch (err) {
      setPathwayImage({ warning: `Failed to load pathway image: ${err.message}` });
    } finally {
      setPathwayLoading(false);
    }
  }

  return (
    <div>
      <ErrorAlert error={error} onDismiss={() => setError(null)} />

      <form onSubmit={handleSubmit}>
        <fieldset style={fsStyle}>
          <legend>Parameters</legend>
          <Row label="Database">
            <select value={database} onChange={e => setDatabase(e.target.value)} style={selStyle}>
              <option value="KEGG">KEGG</option>
              <option value="SMPDB">SMPDB</option>
            </select>
          </Row>
        </fieldset>
        <button type="submit" disabled={loading} style={btnStyle(loading)}>
          {loading ? 'Running…' : 'Run ORA'}
        </button>
      </form>

      {result && (
        <div style={{ marginTop: 20 }}>
          <PlotGrid plots={result.plots} />
          {result.results && result.results.length > 0 && (
            <div style={{ overflowX: 'auto', marginTop: 12 }}>
              <table style={tblStyle}>
                <thead>
                  <tr>
                    {['Pathway ID', 'Pathway Name', 'Matched', 'Pathway Size', 'p-value', 'FDR', ''].map(h => (
                      <th key={h} style={thStyle}>{h}</th>
                    ))}
                  </tr>
                </thead>
                <tbody>
                  {result.results.map((row, i) => (
                    <tr key={i} style={{ background: i % 2 === 0 ? '#f9f9f9' : '#fff' }}>
                      <td style={tdStyle}>{row.pathway_id}</td>
                      <td style={tdStyle}>{row.pathway_name}</td>
                      <td style={tdStyle}>{row.matched}</td>
                      <td style={tdStyle}>{row.pathway_size}</td>
                      <td style={tdStyle}>{fmtNum(row.p_value)}</td>
                      <td style={tdStyle}>{fmtNum(row.fdr)}</td>
                      <td style={tdStyle}>
                        <button
                          onClick={() => viewPathway(row.pathway_id)}
                          style={{ ...btnStyle(false), padding: '3px 10px', fontSize: 12 }}
                        >
                          View Pathway
                        </button>
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}

          {activePathway && (
            <div style={{ marginTop: 16, padding: 12, border: '1px solid #ddd', borderRadius: 4 }}>
              <strong>Pathway: {activePathway}</strong>
              {pathwayLoading && <p>Loading pathway diagram…</p>}
              {pathwayImage && (
                <>
                  {pathwayImage.warning
                    ? <div role="alert" style={warnStyle}>⚠ {pathwayImage.warning}</div>
                    : <img src={pathwayImage.url} alt={`Pathway ${activePathway}`} style={{ maxWidth: '100%', marginTop: 8 }} />
                  }
                  {pathwayImage.note && (
                    <p style={{ fontSize: 12, color: '#666', marginTop: 6 }}>{pathwayImage.note}</p>
                  )}
                </>
              )}
            </div>
          )}
        </div>
      )}
    </div>
  );
}

// ---------------------------------------------------------------------------
// Topology tab
// ---------------------------------------------------------------------------
function TopologyTab({ sessionId }) {
  const [metric, setMetric]   = useState('betweenness');
  const [result, setResult]   = useState(null);
  const [error, setError]     = useState(null);
  const [loading, setLoading] = useState(false);

  async function handleSubmit(e) {
    e.preventDefault();
    setError(null);
    setResult(null);
    setLoading(true);
    try {
      const data = await apiPost(`/${sessionId}/analysis/enrichment/topology`, { topology_metric: metric });
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
      <ErrorAlert error={error} onDismiss={() => setError(null)} />

      <form onSubmit={handleSubmit}>
        <fieldset style={fsStyle}>
          <legend>Parameters</legend>
          <Row label="Topology metric">
            <select value={metric} onChange={e => setMetric(e.target.value)} style={selStyle}>
              <option value="betweenness">Betweenness</option>
              <option value="degree">Degree</option>
            </select>
          </Row>
        </fieldset>
        <button type="submit" disabled={loading} style={btnStyle(loading)}>
          {loading ? 'Running…' : 'Run Topology Analysis'}
        </button>
      </form>

      {result && (
        <div style={{ marginTop: 20 }}>
          <PlotGrid plots={result.plots} />
          {result.results && result.results.length > 0 && (
            <div style={{ overflowX: 'auto', marginTop: 12 }}>
              <table style={tblStyle}>
                <thead>
                  <tr>
                    {['Pathway ID', 'Pathway Name', 'Impact Score', 'p-value', 'FDR'].map(h => (
                      <th key={h} style={thStyle}>{h}</th>
                    ))}
                  </tr>
                </thead>
                <tbody>
                  {result.results.map((row, i) => (
                    <tr key={i} style={{ background: i % 2 === 0 ? '#f9f9f9' : '#fff' }}>
                      <td style={tdStyle}>{row.pathway_id}</td>
                      <td style={tdStyle}>{row.pathway_name}</td>
                      <td style={tdStyle}>{fmtNum(row.impact_score)}</td>
                      <td style={tdStyle}>{fmtNum(row.p_value)}</td>
                      <td style={tdStyle}>{fmtNum(row.fdr)}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}
        </div>
      )}
    </div>
  );
}

// ---------------------------------------------------------------------------
// Lipid Class tab
// ---------------------------------------------------------------------------
function LipidTab({ sessionId }) {
  const [result, setResult]   = useState(null);
  const [error, setError]     = useState(null);
  const [loading, setLoading] = useState(false);

  async function handleSubmit(e) {
    e.preventDefault();
    setError(null);
    setResult(null);
    setLoading(true);
    try {
      const data = await apiPost(`/${sessionId}/analysis/enrichment/lipid-class`, {});
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
      <ErrorAlert error={error} onDismiss={() => setError(null)} />

      <button onClick={handleSubmit} disabled={loading} style={{ ...btnStyle(loading), marginBottom: 16 }}>
        {loading ? 'Running…' : 'Run Lipid Class Enrichment'}
      </button>

      {result && (
        <div style={{ marginTop: 20 }}>
          <PlotGrid plots={result.plots} />
          {result.results && result.results.length > 0 && (
            <div style={{ overflowX: 'auto', marginTop: 12 }}>
              <table style={tblStyle}>
                <thead>
                  <tr>
                    {['Lipid Class', 'Matched', 'Background', 'p-value', 'FDR'].map(h => (
                      <th key={h} style={thStyle}>{h}</th>
                    ))}
                  </tr>
                </thead>
                <tbody>
                  {result.results.map((row, i) => (
                    <tr key={i} style={{ background: i % 2 === 0 ? '#f9f9f9' : '#fff' }}>
                      <td style={tdStyle}>{row.lipid_class}</td>
                      <td style={tdStyle}>{row.matched}</td>
                      <td style={tdStyle}>{row.background}</td>
                      <td style={tdStyle}>{fmtNum(row.p_value)}</td>
                      <td style={tdStyle}>{fmtNum(row.fdr)}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}
        </div>
      )}
    </div>
  );
}

// ---------------------------------------------------------------------------
// Shared helpers
// ---------------------------------------------------------------------------
function PlotGrid({ plots }) {
  if (!plots || plots.length === 0) return null;
  const list = Array.isArray(plots) ? plots : Object.values(plots);
  return (
    <div style={{ display: 'flex', flexWrap: 'wrap', gap: 12, marginTop: 12 }}>
      {list.map((url, i) => (
        <img key={i} src={url} alt={`plot-${i}`} style={{ maxWidth: 320, border: '1px solid #ddd' }} />
      ))}
    </div>
  );
}

function Row({ label, children }) {
  return (
    <div style={{ display: 'flex', alignItems: 'center', marginBottom: 8, gap: 8 }}>
      <span style={{ minWidth: 160 }}>{label}</span>
      {children}
    </div>
  );
}

function fmtNum(n) {
  if (n == null) return '—';
  return typeof n === 'number' ? n.toExponential(3) : n;
}

const headingStyle = { fontSize: 20, fontWeight: 700, color: '#1a1a24', marginBottom: 16, marginTop: 0 };
const fsStyle  = { border: '1px solid #ddd', borderRadius: 4, padding: '10px 14px', marginBottom: 12 };
const selStyle = { padding: '4px 6px' };
const warnStyle = { background: '#fff3cd', border: '1px solid #ffc107', borderRadius: 4, padding: '8px 12px', marginBottom: 12, color: '#856404' };
const tblStyle  = { borderCollapse: 'collapse', width: '100%', fontSize: 13 };
const thStyle   = { background: '#f0f0f0', padding: '6px 10px', textAlign: 'left', borderBottom: '2px solid #ccc' };
const tdStyle   = { padding: '5px 10px', borderBottom: '1px solid #eee' };
const btnStyle  = loading => ({
  padding: '9px 22px',
  background: loading ? '#aaa' : '#7b1c2e',
  color: '#fff',
  border: 'none',
  borderRadius: 5,
  cursor: loading ? 'not-allowed' : 'pointer',
  fontWeight: 600,
  fontSize: 14,
});

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
