// src/components/LandingPage.jsx — InterOmics landing page
// Maroon / white / light-gray design system.

import React, { useState } from 'react';
import logoImg from '../assets/interomics-logo-V1.png.png';

// ─── Design tokens ──────────────────────────────────────────────────────────
const C = {
  maroon:        '#7b1c2e',
  maroonDark:    '#5e1522',
  maroonDeep:    '#4a1019',
  maroonLight:   '#f5eaec',
  white:         '#ffffff',
  bgPage:        '#f4f4f5',
  border:        '#e2e2e5',
  textPrimary:   '#1a1a24',
  textSecondary: '#555563',
  textMuted:     '#888896',
  shadow:        '0 2px 8px rgba(0,0,0,0.07)',
  shadowHover:   '0 8px 24px rgba(0,0,0,0.13)',
};

// ─── Module data ────────────────────────────────────────────────────────────
const GROUPS = [
  {
    id: 'integration',
    label: 'Multi-Omics Integration',
    modules: [
      {
        id: 'diablo',
        title: 'DIABLO',
        subtitle: 'Multi-Omics Integration',
        hover: 'Identifies a joint biomarker signature that discriminates sample groups across 2–4 omics datasets simultaneously.',
        navTarget: 'analyse',
      },
    ],
  },
  {
    id: 'network',
    label: 'Network & Correlation Analysis',
    modules: [
      {
        id: 'corr-single',
        title: 'Single-Dataset Correlation',
        subtitle: 'Intra-Omics Network',
        hover: 'Computes pairwise feature correlations within one dataset and builds a ranked centrality network.',
        navTarget: 'visualise',
      },
      {
        id: 'corr-cross',
        title: 'Cross-Dataset Correlation',
        subtitle: 'Inter-Omics Network',
        hover: 'Identifies correlated features across two different omics layers and highlights cross-dataset hub nodes.',
        navTarget: 'visualise',
      },
    ],
  },
  {
    id: 'enrichment',
    label: 'Enrichment & Interpretation',
    modules: [
      {
        id: 'ora',
        title: 'Metabolite ORA',
        subtitle: 'Over-Representation Analysis',
        hover: 'Tests whether your mapped metabolites are statistically over-represented in KEGG or SMPDB pathways.',
        navTarget: 'visualise',
      },
      {
        id: 'topology',
        title: 'Pathway Topology',
        subtitle: 'Network Impact Analysis',
        hover: 'Weights enriched pathways by the topological centrality of your matched compounds within the pathway graph.',
        navTarget: 'visualise',
      },
      {
        id: 'lipid',
        title: 'Lipid Class Enrichment',
        subtitle: 'Lipidomics Enrichment',
        hover: 'Tests whether specific lipid classes are over-represented in your significant feature list.',
        navTarget: 'visualise',
      },
    ],
  },
  {
    id: 'stats',
    label: 'Statistical Modeling',
    modules: [
      {
        id: 'stats',
        title: 'Statistical Modeling',
        subtitle: 'Feature-Wise Linear Models',
        hover: 'Fits a linear or mixed-effects model per feature, returning standardized betas, partial η², and FDR-adjusted p-values.',
        navTarget: 'visualise',
      },
    ],
  },
];

// ─── Root ───────────────────────────────────────────────────────────────────
export default function LandingPage({ onModuleClick, onStartPipeline }) {
  const [showModules, setShowModules] = useState(false);

  function handleOpen() {
    setShowModules(true);
    setTimeout(() => {
      document.getElementById('module-overview')?.scrollIntoView({ behavior: 'smooth' });
    }, 40);
  }

  return (
    <div style={{ background: C.bgPage, minHeight: '100vh', fontFamily: 'system-ui, sans-serif' }}>
      <HeroSection onOpen={handleOpen} />
      <ModuleOverview visible={showModules} onModuleClick={onModuleClick} />
      <Footer />
    </div>
  );
}

// ─── Hero ────────────────────────────────────────────────────────────────────
function HeroSection({ onOpen }) {
  return (
    <section style={{
      background: C.white,
      borderBottom: `1px solid ${C.border}`,
      padding: '80px 24px 72px',
    }}>
      <style>{`
        @keyframes logoEntrance {
          from { opacity: 0; transform: scale(0.95); }
          to   { opacity: 1; transform: scale(1); }
        }
      `}</style>
      <div style={{
        maxWidth: 600,
        margin: '0 auto',
        textAlign: 'center',
        display: 'flex',
        flexDirection: 'column',
        alignItems: 'center',
        gap: 24,
      }}>
        <img
          src={logoImg}
          alt="InterOmics"
          style={{
            height: 640,
            width: 'auto',
            display: 'block',
            animation: 'logoEntrance 0.55s ease both',
          }}
        />

        <h1 style={{
          fontSize: 28,
          fontWeight: 700,
          color: C.textPrimary,
          lineHeight: 1.42,
          margin: 0,
          letterSpacing: '-0.015em',
        }}>
          From raw omics data to reproducible biological insight in one workflow
        </h1>

        <p style={{
          fontSize: 15,
          color: C.textSecondary,
          lineHeight: 1.65,
          margin: 0,
          maxWidth: 480,
        }}>
          InterOmics integrates preprocessing, network analysis, enrichment, and statistical modeling into a single reproducible pipeline.
        </p>

        <MaroonButton onClick={onOpen} label="Open Module Overview" large />
      </div>
    </section>
  );
}

// ─── Module overview ─────────────────────────────────────────────────────────
function ModuleOverview({ visible, onModuleClick }) {
  return (
    <section
      id="module-overview"
      style={{
        padding: '56px 24px 64px',
        opacity:    visible ? 1 : 0,
        transform:  visible ? 'translateY(0)' : 'translateY(18px)',
        transition: 'opacity 0.35s ease, transform 0.35s ease',
        pointerEvents: visible ? 'auto' : 'none',
      }}
    >
      <div style={{ maxWidth: 1040, margin: '0 auto' }}>
        <div style={{ marginBottom: 44 }}>
          <h2 style={{
            fontSize: 20,
            fontWeight: 800,
            color: C.textPrimary,
            margin: '0 0 8px',
            letterSpacing: '-0.01em',
          }}>
            Analysis Modules
          </h2>
          <p style={{ fontSize: 14, color: C.textSecondary, margin: 0, lineHeight: 1.6 }}>
            Select any module to enter the pipeline at that analysis step.
          </p>
        </div>

        {GROUPS.map(group => (
          <ModuleGroup key={group.id} group={group} onModuleClick={onModuleClick} />
        ))}
      </div>
    </section>
  );
}

function ModuleGroup({ group, onModuleClick }) {
  return (
    <div style={{ marginBottom: 40 }}>
      {/* Group label row */}
      <div style={{ display: 'flex', alignItems: 'center', gap: 12, marginBottom: 18 }}>
        <span style={{
          fontSize: 12,
          fontWeight: 800,
          textTransform: 'uppercase',
          letterSpacing: '0.08em',
          color: C.textPrimary,
          whiteSpace: 'nowrap',
        }}>
          {group.label}
        </span>
        <div style={{ flex: 1, height: 1, background: C.border }} />
      </div>

      {/* Tiles */}
      <div style={{ display: 'flex', flexWrap: 'wrap', gap: 14 }}>
        {group.modules.map(mod => (
          <ModuleTile key={mod.id} mod={mod} onClick={() => onModuleClick(mod)} />
        ))}
      </div>
    </div>
  );
}

function ModuleTile({ mod, onClick }) {
  const [hovered, setHovered] = useState(false);
  const [pressed, setPressed] = useState(false);

  return (
    <div
      role="button"
      tabIndex={0}
      aria-label={mod.title}
      onClick={onClick}
      onKeyDown={e => (e.key === 'Enter' || e.key === ' ') && onClick()}
      onMouseEnter={() => setHovered(true)}
      onMouseLeave={() => { setHovered(false); setPressed(false); }}
      onMouseDown={() => setPressed(true)}
      onMouseUp={() => setPressed(false)}
      onFocus={() => setHovered(true)}
      onBlur={() => setHovered(false)}
      style={{
        position: 'relative',
        width: 192,
        minHeight: 106,
        background: C.white,
        border: `1px solid ${C.border}`,
        borderRadius: 8,
        padding: '16px 16px 14px',
        cursor: 'pointer',
        overflow: 'hidden',
        transition: 'transform 0.15s ease, box-shadow 0.15s ease',
        display: 'flex',
        flexDirection: 'column',
        gap: 8,
        userSelect: 'none',
        outline: 'none',
        boxShadow: hovered ? C.shadowHover : C.shadow,
        transform: pressed
          ? 'translateY(0) scale(0.97)'
          : hovered
            ? 'translateY(-4px) scale(1)'
            : 'translateY(0) scale(1)',
      }}
    >
      <ModuleIcon id={mod.id} />

      <div style={{ fontSize: 13, fontWeight: 700, color: C.textPrimary, lineHeight: 1.3 }}>
        {mod.title}
      </div>
      <div style={{ fontSize: 11, color: C.textMuted, lineHeight: 1.3 }}>
        {mod.subtitle}
      </div>

      {/* Hover overlay */}
      <div
        aria-hidden="true"
        style={{
          position: 'absolute',
          inset: 0,
          background: 'rgba(58, 8, 18, 0.91)',
          padding: '14px 16px',
          display: 'flex',
          flexDirection: 'column',
          justifyContent: 'center',
          gap: 10,
          transition: 'opacity 0.18s ease',
          borderRadius: 7,
          opacity: hovered ? 1 : 0,
          pointerEvents: hovered ? 'auto' : 'none',
        }}
      >
        <p style={{ fontSize: 12, color: '#f0e8ea', margin: 0, lineHeight: 1.5 }}>
          {mod.hover}
        </p>
        <span style={{ fontSize: 11, color: '#e8a0aa', fontWeight: 600 }}>Open →</span>
      </div>
    </div>
  );
}

// ─── Buttons ─────────────────────────────────────────────────────────────────
function MaroonButton({ onClick, label, large }) {
  const [hovered, setHovered] = useState(false);
  const [pressed, setPressed] = useState(false);
  return (
    <button
      onClick={onClick}
      onMouseEnter={() => setHovered(true)}
      onMouseLeave={() => { setHovered(false); setPressed(false); }}
      onMouseDown={() => setPressed(true)}
      onMouseUp={() => setPressed(false)}
      style={{
        padding: large ? '14px 36px' : '11px 28px',
        background: pressed ? C.maroonDeep : hovered ? C.maroonDark : C.maroon,
        color: C.white,
        border: 'none',
        borderRadius: 6,
        fontSize: large ? 15 : 14,
        fontWeight: 600,
        cursor: 'pointer',
        letterSpacing: '0.02em',
        transition: 'background 0.14s ease, transform 0.1s ease',
        transform: pressed ? 'scale(0.97)' : 'scale(1)',
      }}
    >
      {label}
    </button>
  );
}

// ─── Footer ───────────────────────────────────────────────────────────────────
function Footer() {
  return (
    <footer style={{
      borderTop: `1px solid ${C.border}`,
      background: C.white,
      padding: '20px 24px',
      textAlign: 'center',
    }}>
      <span style={{ fontSize: 13, color: C.textSecondary, letterSpacing: '0.01em' }}>
        InterOmics is built to make rigorous multi-omics analysis reproducible, transparent, and accessible to every researcher.
      </span>
    </footer>
  );
}

// ─── Per-module SVG icons ─────────────────────────────────────────────────────
function ModuleIcon({ id }) {
  const s = { width: 26, height: 26 };
  switch (id) {
    case 'diablo':
      return (
        <svg {...s} viewBox="0 0 26 26" fill="none" aria-hidden="true">
          <circle cx="8"  cy="13" r="5" stroke={C.maroon} strokeWidth="1.8" fill={C.maroonLight} />
          <circle cx="18" cy="13" r="5" stroke={C.maroon} strokeWidth="1.8" fill={C.maroonLight} />
          <line x1="13" y1="13" x2="13" y2="13" stroke={C.maroon} strokeWidth="2.5" strokeLinecap="round" />
        </svg>
      );
    case 'corr-single':
      return (
        <svg {...s} viewBox="0 0 26 26" fill="none" aria-hidden="true">
          <circle cx="13" cy="13" r="3.5" fill={C.maroon} />
          <circle cx="5"  cy="5"  r="2.2" fill={C.maroon} opacity="0.45" />
          <circle cx="21" cy="5"  r="2.2" fill={C.maroon} opacity="0.45" />
          <circle cx="5"  cy="21" r="2.2" fill={C.maroon} opacity="0.45" />
          <circle cx="21" cy="21" r="2.2" fill={C.maroon} opacity="0.45" />
          <line x1="13" y1="13" x2="5"  y2="5"  stroke={C.maroon} strokeWidth="1.3" opacity="0.55" />
          <line x1="13" y1="13" x2="21" y2="5"  stroke={C.maroon} strokeWidth="1.3" opacity="0.55" />
          <line x1="13" y1="13" x2="5"  y2="21" stroke={C.maroon} strokeWidth="1.3" opacity="0.55" />
          <line x1="13" y1="13" x2="21" y2="21" stroke={C.maroon} strokeWidth="1.3" opacity="0.55" />
        </svg>
      );
    case 'corr-cross':
      return (
        <svg {...s} viewBox="0 0 26 26" fill="none" aria-hidden="true">
          <circle cx="5"  cy="9"  r="2.2" fill={C.maroon} opacity="0.5" />
          <circle cx="5"  cy="17" r="2.2" fill={C.maroon} opacity="0.5" />
          <circle cx="21" cy="9"  r="2.2" fill={C.maroon} opacity="0.5" />
          <circle cx="21" cy="17" r="2.2" fill={C.maroon} opacity="0.5" />
          <line x1="5"  y1="9"  x2="21" y2="17" stroke={C.maroon} strokeWidth="1.5" />
          <line x1="5"  y1="17" x2="21" y2="9"  stroke={C.maroon} strokeWidth="1.5" />
        </svg>
      );
    case 'ora':
      return (
        <svg {...s} viewBox="0 0 26 26" fill="none" aria-hidden="true">
          {[20, 15, 10, 6].map((w, i) => (
            <rect key={i} x={3} y={4 + i * 5.2} width={w} height={3.8} rx={1.4}
              fill={C.maroon} opacity={1 - i * 0.2} />
          ))}
        </svg>
      );
    case 'topology':
      return (
        <svg {...s} viewBox="0 0 26 26" fill="none" aria-hidden="true">
          <circle cx="13" cy="4"  r="2.8" fill={C.maroon} />
          <circle cx="4"  cy="19" r="2.8" fill={C.maroon} opacity="0.5" />
          <circle cx="22" cy="19" r="2.8" fill={C.maroon} opacity="0.5" />
          <line x1="13" y1="7"  x2="4"  y2="16" stroke={C.maroon} strokeWidth="1.5" />
          <line x1="13" y1="7"  x2="22" y2="16" stroke={C.maroon} strokeWidth="1.5" />
          <line x1="4"  y1="19" x2="22" y2="19" stroke={C.maroon} strokeWidth="1.5" opacity="0.45" />
        </svg>
      );
    case 'lipid':
      return (
        <svg {...s} viewBox="0 0 26 26" fill="none" aria-hidden="true">
          <ellipse cx="13" cy="13" rx="9" ry="5.5" stroke={C.maroon} strokeWidth="1.7" fill={C.maroonLight} />
          <ellipse cx="13" cy="13" rx="9" ry="5.5" stroke={C.maroon} strokeWidth="1.7" fill="none"
            transform="rotate(60 13 13)" opacity="0.45" />
          <ellipse cx="13" cy="13" rx="9" ry="5.5" stroke={C.maroon} strokeWidth="1.7" fill="none"
            transform="rotate(120 13 13)" opacity="0.25" />
        </svg>
      );
    case 'stats':
      return (
        <svg {...s} viewBox="0 0 26 26" fill="none" aria-hidden="true">
          <circle cx="9"  cy="7"  r="2"   fill={C.maroon} />
          <circle cx="17" cy="6"  r="2"   fill={C.maroon} />
          <circle cx="6"  cy="17" r="1.5" fill={C.maroon} opacity="0.4" />
          <circle cx="13" cy="19" r="1.5" fill={C.maroon} opacity="0.4" />
          <circle cx="20" cy="16" r="1.5" fill={C.maroon} opacity="0.4" />
          <line x1="3"  y1="22" x2="23" y2="22" stroke={C.maroon} strokeWidth="1.4" opacity="0.25" />
          <line x1="13" y1="3"  x2="13" y2="22" stroke={C.maroon} strokeWidth="1.4" opacity="0.25" strokeDasharray="2 2" />
        </svg>
      );
    default:
      return null;
  }
}
