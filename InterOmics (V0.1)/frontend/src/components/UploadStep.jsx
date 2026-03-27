// src/components/UploadStep.jsx — DIABLO Upload Data step

import { useState } from 'react';
import { apiPostForm } from '../api/session';
import ErrorAlert from './ErrorAlert';
import HelpTooltip from './HelpTooltip';

export default function UploadStep({ sessionId, onComplete }) {
  const [matrices,    setMatrices]    = useState([]);
  const [metadata,    setMetadata]    = useState(null);
  const [result,      setResult]      = useState(null);
  const [error,       setError]       = useState(null);
  const [loading,     setLoading]     = useState(false);
  const [matrixError, setMatrixError] = useState('');
  const [metaError,   setMetaError]   = useState('');

  const canSubmit = matrices.length >= 1 && matrices.length <= 4 && metadata !== null;

  function handleMatrixChange(e) {
    const files = Array.from(e.target.files);
    if (files.length > 4) {
      setMatrixError('Maximum 4 feature matrix files allowed.');
      setMatrices(files.slice(0, 4));
    } else {
      setMatrixError('');
      setMatrices(files);
    }
  }

  function handleMetaChange(e) {
    const f = e.target.files[0] || null;
    if (f && !f.name.endsWith('.csv')) {
      setMetaError('Metadata file must be a CSV.');
      setMetadata(null);
    } else {
      setMetaError('');
      setMetadata(f);
    }
  }

  async function handleSubmit(e) {
    e.preventDefault();
    if (!sessionId) {
      setError({ error_code: 'NO_SESSION', detail: 'No active session. Please go back to the module overview and try again.', recoverable: false });
      return;
    }
    setError(null);
    setResult(null);
    setLoading(true);

    const form = new FormData();
    matrices.forEach((f, i) => form.append(`matrices[${i}]`, f));
    form.append('metadata', metadata);

    try {
      const data = await apiPostForm(`/${sessionId}/upload`, form);
      if (data.status === 'error') {
        setError(data);
      } else {
        setResult(data);
        onComplete(data.files_received);
      }
    } catch (err) {
      setError({ error_code: 'NETWORK_ERROR', detail: err.message, recoverable: true });
    } finally {
      setLoading(false);
    }
  }

  return (
    <div>
      <h2 style={headingStyle}>Upload Data</h2>

      {/* Outputs box — Upload page only */}
      <div style={infoBoxStyle}>
        <p style={infoLabelStyle}>Outputs</p>
        <ul style={{ margin: 0, paddingLeft: 18 }}>
          {[
            'Multi-omics biomarker signature — selected features per block and component',
            'Model performance metrics: balanced error rate (BER) and confusion matrix',
            'Sample score plots (consensus and per-block) and feature loading plots',
            'Cross-omics correlation network and circos plot',
            'Downloadable selected-feature and ranked-feature tables (CSV)',
          ].map((o, i) => (
            <li key={i} style={{ fontSize: 13, color: '#555563', marginBottom: 2 }}>{o}</li>
          ))}
        </ul>
      </div>

      <ErrorAlert error={error} onDismiss={() => setError(null)} />

      <form onSubmit={handleSubmit}>
        <div style={{ marginBottom: 16 }}>
          <label style={{ fontWeight: 600, fontSize: 14, display: 'flex', alignItems: 'center' }}>
            Feature Matrix files (1–4 CSV files)
            <HelpTooltip text="Each CSV must have a SampleID column and one column per feature. Upload one file per omics layer (e.g. metabolomics, proteomics)." />
          </label>
          <input type="file" accept=".csv" multiple onChange={handleMatrixChange} style={{ marginTop: 6, display: 'block' }} />
          {matrixError && <p style={fieldErrStyle}>{matrixError}</p>}
          {matrices.length > 0 && (
            <ul style={{ margin: '4px 0 0', paddingLeft: 20, fontSize: 13 }}>
              {matrices.map(f => <li key={f.name}>{f.name}</li>)}
            </ul>
          )}
        </div>

        <div style={{ marginBottom: 16 }}>
          <label style={{ fontWeight: 600, fontSize: 14, display: 'flex', alignItems: 'center' }}>
            Metadata file (1 CSV file)
            <HelpTooltip text="Must contain a SampleID column matching your matrix files, plus at least one grouping or predictor column." />
          </label>
          <input type="file" accept=".csv" onChange={handleMetaChange} style={{ marginTop: 6, display: 'block' }} />
          {metaError && <p style={fieldErrStyle}>{metaError}</p>}
          {metadata && <p style={{ margin: '4px 0 0', fontSize: 13 }}>{metadata.name}</p>}
        </div>

        <button type="submit" disabled={!canSubmit || loading} style={btnStyle(!canSubmit || loading)}>
          {loading ? 'Uploading…' : 'Upload'}
        </button>
      </form>

      {result && (
        <div style={successStyle}>
          <strong>Upload successful.</strong> Files received:
          <ul style={{ margin: '4px 0 0', paddingLeft: 20 }}>
            {result.files_received.map(f => <li key={f}>{f}</li>)}
          </ul>
        </div>
      )}
    </div>
  );
}

const headingStyle   = { fontSize: 20, fontWeight: 700, color: '#1a1a24', margin: '0 0 14px' };
const infoBoxStyle   = { background: '#f5eaec', border: '1px solid #e2c8cc', borderRadius: 6, padding: '10px 14px', marginBottom: 18, maxWidth: 640, textAlign: 'left' };
const infoLabelStyle = { fontSize: 12, fontWeight: 700, color: '#7b1c2e', margin: '0 0 6px', textTransform: 'uppercase', letterSpacing: '0.06em' };
const fieldErrStyle  = { margin: '4px 0 0', fontSize: 12, color: '#c0392b' };
const successStyle   = { marginTop: 16, padding: 12, background: '#d4edda', border: '1px solid #c3e6cb', borderRadius: 4, maxWidth: 640 };
const btnStyle = disabled => ({
  padding: '9px 22px',
  background: disabled ? '#aaa' : '#7b1c2e',
  color: '#fff',
  border: 'none',
  borderRadius: 5,
  cursor: disabled ? 'not-allowed' : 'pointer',
  fontWeight: 600,
  fontSize: 14,
});
