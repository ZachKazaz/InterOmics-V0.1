// src/components/CorrelationStep.jsx — Correlation analysis step
// Task 4.8
// Interactive network uses Cytoscape.js for client-side threshold filtering.

import React, { useState, useEffect, useRef } from 'react';
import { apiPost } from '../api/session';
import ErrorAlert from './ErrorAlert';
import EducationPanel from './EducationPanel';
import Breadcrumb from './Breadcrumb';
import HelpTooltip from './HelpTooltip';

const EDUCATION = {
  what_it_does: 'Computes pairwise feature correlations within a single dataset or between two datasets, builds a network, and ranks nodes by centrality.',
  when_to_use: 'After preprocessing. Use single-dataset for intra-omics networks; cross-dataset for inter-omics connections.',
  input_requirements: 'Preprocessed feature matrices. Cross-dataset requires the same scaling method applied to both datasets.',
  output_interpretation: 'Edge list (feature pairs above threshold), node metrics (degree, betweenness, closeness), and network plots.',
  common_mistakes: [
    'Partial correlation with >1000 features is blocked — apply additional filtering first.',
    'Cross-dataset correlation requires matching scaling methods on both datasets.',
  ],
};

const METHODS    = ['pearson', 'spearman', 'partial'];
const DIRECTIONS = ['both', 'positive', 'negative'];
const CENTRALITY = ['degree', 'betweenness', 'closeness'];

export default function CorrelationStep({ sessionId, datasets, onGoHome }) {
  const [tab, setTab] = useState('single');

  return (
    <div>
      <Breadcrumb crumbs={[
        { label: 'Home', onClick: onGoHome },
        { label: 'Correlation Analysis' },
      ]} />
      <h2 style={headingStyle}>Correlation Analysis</h2>

      <OutputsBox outputs={[
        'Edge list of correlated feature pairs above threshold',
        'Node centrality metrics (degree, betweenness, closeness)',
        'Interactive network visualization with adjustable threshold',
        'Downloadable edge list and node metrics CSVs',
      ]} />

      <EducationPanel education={EDUCATION} />

      <div style={{ display: 'flex', gap: 0, marginBottom: 16 }}>
        {['single', 'cross'].map(t => (
          <button
            key={t}
            onClick={() => setTab(t)}
            style={{
              padding: '8px 20px',
              background: tab === t ? '#7b1c2e' : '#f4f4f5',
              color: tab === t ? '#fff' : '#1a1a24',
              border: '1px solid #e2e2e5',
              borderLeft: t === 'cross' ? 'none' : '1px solid #e2e2e5',
              cursor: 'pointer',
              fontWeight: tab === t ? 700 : 400,
              fontSize: 13,
            }}
          >
            {t === 'single' ? 'Single Dataset' : 'Cross-Dataset'}
          </button>
        ))}
      </div>

      {tab === 'single'
        ? <SingleCorrelation sessionId={sessionId} datasets={datasets} />
        : <CrossCorrelation  sessionId={sessionId} datasets={datasets} />
      }
    </div>
  );
}

// ---------------------------------------------------------------------------
// Single-dataset correlation
// ---------------------------------------------------------------------------
function SingleCorrelation({ sessionId, datasets }) {
  const [params, setParams] = useState({
    dataset: datasets?.[0] || '',
    method: 'pearson',
    threshold: 0.7,
    direction: 'both',
    centrality_rank_metric: 'degree',
  });
  const [result, setResult]     = useState(null);
  const [edgeList, setEdgeList] = useState([]);
  const [error, setError]       = useState(null);
  const [warning, setWarning]   = useState(null);
  const [loading, setLoading]   = useState(false);
  const [threshold, setThreshold] = useState(0.7);

  async function handleSubmit(e) {
    e.preventDefault();
    setError(null);
    setWarning(null);
    setResult(null);
    setLoading(true);

    try {
      const data = await apiPost(`/${sessionId}/analysis/correlation/single`, params);
      const payload = data.data || data;

      if (payload.status === 'error') {
        setError(payload);
      } else {
        if (payload.warning) setWarning(payload.warning);
        setResult(payload);
        // Load full edge list for interactive filtering
        if (payload.edge_list_data) {
          setEdgeList(payload.edge_list_data);
        }
      }
    } catch (err) {
      setError({ error_code: 'NETWORK_ERROR', detail: err.message, recoverable: true });
    } finally {
      setLoading(false);
    }
  }

  return (
    <div>
      {warning && (
        <div role="alert" style={warnStyle}>⚠ {warning}</div>
      )}
      <ErrorAlert error={error} onDismiss={() => setError(null)} />

      <form onSubmit={handleSubmit}>
        <fieldset style={fsStyle}>
          <legend>Parameters</legend>
          <Row label="Dataset">
            <select value={params.dataset}
              onChange={e => setParams(p => ({ ...p, dataset: e.target.value }))}
              style={selStyle}>
              {(datasets || []).map(d => <option key={d} value={d}>{d}</option>)}
            </select>
          </Row>
          <Row label="Method">
            <select value={params.method}
              onChange={e => setParams(p => ({ ...p, method: e.target.value }))}
              style={selStyle}>
              {METHODS.map(m => <option key={m} value={m}>{m}</option>)}
            </select>
            <HelpTooltip text="Pearson: linear correlation. Spearman: rank-based. Partial: conditions on all other features (blocked for >1000 features)." />
          </Row>
          <Row label={`Threshold (${params.threshold})`}>
            <input type="range" min={0} max={1} step={0.05}
              value={params.threshold}
              onChange={e => setParams(p => ({ ...p, threshold: parseFloat(e.target.value) }))}
              style={{ width: 160 }} />
            <HelpTooltip text="Only feature pairs with |correlation| above this value appear in the network." />
          </Row>
          <Row label="Direction">
            <select value={params.direction}
              onChange={e => setParams(p => ({ ...p, direction: e.target.value }))}
              style={selStyle}>
              {DIRECTIONS.map(d => <option key={d} value={d}>{d}</option>)}
            </select>
          </Row>
          <Row label="Centrality metric">
            <select value={params.centrality_rank_metric}
              onChange={e => setParams(p => ({ ...p, centrality_rank_metric: e.target.value }))}
              style={selStyle}>
              {CENTRALITY.map(c => <option key={c} value={c}>{c}</option>)}
            </select>
          </Row>
        </fieldset>
        <button type="submit" disabled={loading} style={btnStyle(loading)}>
          {loading ? 'Running…' : 'Run Correlation'}
        </button>
      </form>

      {result && (
        <CorrelationResults
          result={result}
          edgeList={edgeList}
          threshold={threshold}
          onThresholdChange={setThreshold}
        />
      )}
    </div>
  );
}

// ---------------------------------------------------------------------------
// Cross-dataset correlation
// ---------------------------------------------------------------------------
function CrossCorrelation({ sessionId, datasets }) {
  const [params, setParams] = useState({
    dataset_a: datasets?.[0] || '',
    dataset_b: datasets?.[1] || '',
    method: 'pearson',
    threshold: 0.7,
    direction: 'both',
  });
  const [result, setResult] = useState(null);
  const [error, setError]   = useState(null);
  const [loading, setLoading] = useState(false);

  async function handleSubmit(e) {
    e.preventDefault();
    setError(null);
    setResult(null);
    setLoading(true);

    try {
      const data = await apiPost(`/${sessionId}/analysis/correlation/cross`, params);
      const payload = data.data || data;
      if (payload.status === 'error') {
        setError(payload);
      } else {
        setResult(payload);
      }
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
          <Row label="Dataset A">
            <select value={params.dataset_a}
              onChange={e => setParams(p => ({ ...p, dataset_a: e.target.value }))}
              style={selStyle}>
              {(datasets || []).map(d => <option key={d} value={d}>{d}</option>)}
            </select>
          </Row>
          <Row label="Dataset B">
            <select value={params.dataset_b}
              onChange={e => setParams(p => ({ ...p, dataset_b: e.target.value }))}
              style={selStyle}>
              {(datasets || []).map(d => <option key={d} value={d}>{d}</option>)}
            </select>
          </Row>
          <Row label="Method">
            <select value={params.method}
              onChange={e => setParams(p => ({ ...p, method: e.target.value }))}
              style={selStyle}>
              {METHODS.filter(m => m !== 'partial').map(m => <option key={m} value={m}>{m}</option>)}
            </select>
          </Row>
          <Row label={`Threshold (${params.threshold})`}>
            <input type="range" min={0} max={1} step={0.05}
              value={params.threshold}
              onChange={e => setParams(p => ({ ...p, threshold: parseFloat(e.target.value) }))}
              style={{ width: 160 }} />
          </Row>
          <Row label="Direction">
            <select value={params.direction}
              onChange={e => setParams(p => ({ ...p, direction: e.target.value }))}
              style={selStyle}>
              {DIRECTIONS.map(d => <option key={d} value={d}>{d}</option>)}
            </select>
          </Row>
        </fieldset>
        <button type="submit" disabled={loading} style={btnStyle(loading)}>
          {loading ? 'Running…' : 'Run Cross-Dataset Correlation'}
        </button>
      </form>

      {result && <PlotGrid plots={result.plots} />}
    </div>
  );
}

// ---------------------------------------------------------------------------
// Shared result display with interactive Cytoscape network
// ---------------------------------------------------------------------------
function CorrelationResults({ result, edgeList, threshold, onThresholdChange }) {
  const cyRef = useRef(null);
  const cyInstance = useRef(null);

  // Filter edges client-side when threshold changes
  useEffect(() => {
    if (!cyInstance.current || edgeList.length === 0) return;
    const filtered = edgeList.filter(e => Math.abs(e.correlation) >= threshold);
    const nodeIds = new Set(filtered.flatMap(e => [e.feature1, e.feature2]));
    cyInstance.current.elements().remove();
    cyInstance.current.add([
      ...Array.from(nodeIds).map(id => ({ data: { id } })),
      ...filtered.map(e => ({
        data: {
          id: `${e.feature1}__${e.feature2}`,
          source: e.feature1,
          target: e.feature2,
          correlation: e.correlation,
        },
      })),
    ]);
    cyInstance.current.layout({ name: 'cose' }).run();
  }, [threshold, edgeList]);

  // Mount Cytoscape when edgeList arrives
  useEffect(() => {
    if (edgeList.length === 0 || !cyRef.current) return;
    import('cytoscape').then(({ default: cytoscape }) => {
      if (cyInstance.current) cyInstance.current.destroy();
      const filtered = edgeList.filter(e => Math.abs(e.correlation) >= threshold);
      const nodeIds = new Set(filtered.flatMap(e => [e.feature1, e.feature2]));
      cyInstance.current = cytoscape({
        container: cyRef.current,
        elements: [
          ...Array.from(nodeIds).map(id => ({ data: { id } })),
          ...filtered.map(e => ({
            data: {
              id: `${e.feature1}__${e.feature2}`,
              source: e.feature1,
              target: e.feature2,
            },
          })),
        ],
        style: [
          { selector: 'node', style: { label: 'data(id)', 'font-size': 8, width: 20, height: 20 } },
          { selector: 'edge', style: { width: 1, 'line-color': '#aaa' } },
        ],
        layout: { name: 'cose' },
      });
    });
    return () => { if (cyInstance.current) cyInstance.current.destroy(); };
  }, [edgeList]);

  return (
    <div style={{ marginTop: 20 }}>
      {edgeList.length > 0 && (
        <div style={{ marginBottom: 12 }}>
          <label>
            <strong>Interactive threshold: {threshold}</strong>
            <input type="range" min={0} max={1} step={0.05}
              value={threshold}
              onChange={e => onThresholdChange(parseFloat(e.target.value))}
              style={{ marginLeft: 10, width: 200 }} />
          </label>
          <div
            ref={cyRef}
            style={{ width: '100%', height: 400, border: '1px solid #ddd', borderRadius: 4, marginTop: 8 }}
          />
        </div>
      )}

      <PlotGrid plots={result.plots} />

      {result.edge_list && (
        <p><a href={result.edge_list} download>Download edge list (CSV)</a></p>
      )}
      {result.node_metrics && (
        <p><a href={result.node_metrics} download>Download node metrics (CSV)</a></p>
      )}
    </div>
  );
}

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

const headingStyle = { fontSize: 20, fontWeight: 700, color: '#1a1a24', marginBottom: 16, marginTop: 0 };
const fsStyle  = { border: '1px solid #ddd', borderRadius: 4, padding: '10px 14px', marginBottom: 12 };
const selStyle = { padding: '4px 6px' };
const warnStyle = { background: '#fff3cd', border: '1px solid #ffc107', borderRadius: 4, padding: '8px 12px', marginBottom: 12, color: '#856404' };
const btnStyle = loading => ({
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
