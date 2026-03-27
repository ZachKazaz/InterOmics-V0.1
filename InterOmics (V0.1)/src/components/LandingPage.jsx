// src/components/LandingPage.jsx — InterOmics landing page
// Maroon / white / light-gray design system.
// Hover overlays use React state + CSS transition — no extra dependencies.

import React, { useState } from 'react';

// ─── Design tokens ─────────────────────────────────────────────────────────
const C = {
  maroon:       '#7b1c2e',
  maroonDark:   '#5e1522',
  maroonLight:  '#f5eaec',
  white:        '#ffffff',
  bgPage:       '#f4f4f5',
  bgCard:       '#ffffff',
  border:       '#e2e2e5',
  textPrimary:  '#1a1a24',
  textSecondary:'#555563',
  textMuted:    '#888896',
  shadow:       '0 2px 8px rgba(0,0,0,0.07)',
  shadowHover:  '0 8px 24px rgba(0,0,0,0.12)',
};

// ─── Module definitions ────────────────────────────────────────────────────
const GROUPS = [
  {
    id: 'integration',
    label: 'Multi-Omics Integration',
    modules: [
      {
        id: 'diablo',
        icon: <DiabloIcon />,
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
        icon: <NetworkIcon />,
        title: 'Single-Dataset Correlation',
        subtitle: 'Intra-Omics Network',
        hover: 'Computes pairwise feature correlations within one dataset and builds a ranked centrality network.',
        navTarget: 'visualise',
      },
      {
        id: 'corr-cross',
        icon: <CrossIcon />,
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
        icon: <ORAIcon />,
        title: 'Metabolite ORA',
        subtitle: 'Over-Representation Analysis',
        hover: 'Tests whether your mapped metabolites are statistically over-represented in KEGG or SMPDB pathways.',
        navTarget: 'visualise',
      },
      {
        id: 'topology',
        icon: <TopologyIcon />,
        title: 'Pathway Topology',
        subtitle: 'Network Impact Analysis',
        hover: 'Weights enriched pathways by the topological centrality of your matched compounds within the pathway graph.',
        navTarget: 'visualise',
      },
      {
        id: 'lipid',
        icon: <LipidIcon />,
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
        icon: <StatsIcon />,
        title: 'Statistical Modeling',
        subtitle: 'Feature-Wise Linear Models',
        hover: 'Fits a linear or mixed-effects model per feature, returning standardized betas, partial η², and FDR-adjusted p-values.',
        navTarget: 'visualise',
      },
    ],
  },
];

// ─── Main component ────────────────────────────────────────────────────────
export default function LandingPage({ onModuleClick, onStartPipeline }) {
  const [showModules, setShowModules] = useState(false);

  function handleOpen() {
    setShowModules(true);
    setTimeout(() => {
      document.getElementById('module-overview')?.scrollIntoView({ behavior: 'smooth' });
    }, 50);
  }

  return (
    <div style={{ background: C.bgPage, minHeight: '100vh' }}>
      <HeroSection onOpen={handleOpen} onStartPipeline={onStartPipeline} />
      <ModuleOverview
        visible={showModules}
        onModuleClick={onModuleClick}
        onStartPipeline={onStartPipeline}
      />
      <Footer />
    </div>
  );
}

// ─── Hero ──────────────────────────────────────────────────────────────────
function HeroSection({ onOpen, onStartPipeline }) {
  return (
    <section style={heroSection}>
      <div style={heroInner}>
        {/* Logo placeholder */}
        <div style={logoArea}>
          <LogoMark />
        </div>

        <h1 style={heroHeading}>
          From raw omics data to reproducible biological insight —<br />
          in one integrated workflow.
        </h1>

        <p style={heroBody}>
          InterOmics integrates multi-omics preprocessing, network analysis, pathway
          enrichment, and statistical modeling into a single, reproducible pipeline.
        </p>

        <div style={{ display: 'flex', gap: 12, justifyContent: 'center', flexWrap: 'wrap' }}>
          <MaroonButton onClick={onOpen} label="Open Module Overview" />
          <GhostButton  onClick={onStartPipeline} label="Start Pipeline →" />
        </div>
      </div>
    </section>
  );
}

// ─── Module overview ───────────────────────────────────────────────────────
function ModuleOverview({ visible, onModuleClick, onStartPipeline }) {
  return (
    <section
      id="module-overview"
      style={{
        ...moduleSection,
        opacity:    visible ? 1 : 0,
        transform:  visible ? 'translateY(0)' : 'translateY(16px)',
        transition: 'opacity 0.35s ease, transform 0.35s ease',
        pointerEvents: visible ? 'auto' : 'none',
      }}
    >
      <div style={sectionInner}>
        <div style={sectionHeader}>
          <h2 style={sectionTitle}>Analysis Modules</h2>
          <p style={sectionSub}>
            Select any module to enter the pipeline at that analysis step.
            All modules require completed preprocessing.
          </p>
        </div>

        {GROUPS.map(group => (
          <ModuleGroup key={group.id} group={group} onModuleClick={onModuleClick} />
        ))}

        <div style={{ textAlign: 'center', marginTop: 40 }}>
          <MaroonButton onClick={onStartPipeline} label="Start Pipeline from Upload →" />
        </div>
      </div>
    </section>
  );
}

function ModuleGroup({ group, onModuleClick }) {
  return (
    <div style={groupBlock}>
      <div style={groupLabelRow}>
        <span style={groupLabelText}>{group.label}</span>
        <div style={groupRule} />
      </div>
      <div style={tilesGrid}>
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
        ...tileBase,
        boxShadow: hovered ? C.shadowHover : C.shadow,
        transform: pressed
          ? 'translateY(0) scale(0.98)'
          : hovered
            ? 'translateY(-4px) scale(1)'
            : 'translateY(0) scale(1)',
      }}
    >
      {/* Icon */}
      <div style={tileIconWrap}>{mod.icon}</div>

      {/* Text */}
      <div style={tileTitleText}>{mod.title}</div>
      <div style={tileSubText}>{mod.subtitle}</div>

      {/* Hover overlay */}
      <div
        aria-hidden="true"
        style={{
          ...overlayBase,
          opacity: hovered ? 1 : 0,
          pointerEvents: hovered ? 'auto' : 'none',
        }}
      >
        <p style={overlayText}>{mod.hover}</p>
        <span style={overlayAction}>Open →</span>
      </div>
    </div>
  );
}

// ─── Footer ────────────────────────────────────────────────────────────────
function Footer() {
  return (
    <footer style={footerStyle}>
      <span style={footerText}>
        InterOmics · Reproducible multi-omics analysis · Built for biological discovery
      </span>
    </footer>
  );
}

// ─── Buttons ───────────────────────────────────────────────────────────────
function MaroonButton({ onClick, label }) {
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
        padding: '11px 28px',
        background: pressed ? '#4a1019' : hovered ? C.maroonDark : C.maroon,
        color: C.white,
        border: 'none',
        borderRadius: 6,
        fontSize: 14,
        fontWeight: 600,
        cursor: 'pointer',
        letterSpacing: '0.02em',
        transition: 'background 0.15s ease, transform 0.1s ease',
        transform: pressed ? 'scale(0.97)' : 'scale(1)',
      }}
    >
      {label}
    </button>
  );
}

function GhostButton({ onClick, label }) {
  const [hovered, setHovered] = useState(false);
  return (
    <button
      onClick={onClick}
      onMouseEnter={() => setHovered(true)}
      onMouseLeave={() => setHovered(false)}
      style={{
        padding: '11px 28px',
        background: hovered ? 'rgba(123,28,46,0.06)' : 'transparent',
        color: C.maroon,
        border: `1.5px solid ${C.maroon}`,
        borderRadius: 6,
        fontSize: 14,
        fontWeight: 600,
        cursor: 'pointer',
        letterSpacing: '0.02em',
        transition: 'background 0.15s ease',
      }}
    >
      {label}
    </button>
  );
}

// ─── SVG icons (inline, no deps) ───────────────────────────────────────────
function LogoMark() {
  // Minimal circos-style ring with arc segments — placeholder for custom logo
  return (
    <svg width="56" height="56" viewBox="0 0 56 56" fill="none" aria-hidden="true">
      <circle cx="28" cy="28" r="24" stroke={C.maroon} strokeWidth="2.5" fill="none" opacity="0.15" />
      <path d="M28 4 A24 24 0 0 1 52 28" stroke={C.maroon} strokeWidth="3" strokeLinecap="round" fill="none" />
      <path d="M52 28 A24 24 0 0 1 28 52" stroke={C.maroon} strokeWidth="3" strokeLinecap="round" fill="none" opacity="0.6" />
      <path d="M28 52 A24 24 0 0 1 4 28" stroke={C.maroon} strokeWidth="3" strokeLinecap="round" fill="none" opacity="0.35" />
      <circle cx="28" cy="28" r="6" fill={C.maroon} opacity="0.9" />
      <line x1="28" y1="22" x2="28" y2="4"  stroke={C.maroon} strokeWidth="1.5" opacity="0.4" />
      <line x1="28" y1="34" x2="28" y2="52" stroke={C.maroon} strokeWidth="1.5" opacity="0.4" />
      <line x1="22" y1="28" x2="4"  y2="28" stroke={C.maroon} strokeWidth="1.5" opacity="0.4" />
      <line x1="34" y1="28" x2="52" y2="28" stroke={C.maroon} strokeWidth="1.5" opacity="0.4" />
    </svg>
  );
}

function DiabloIcon() {
  return (
    <svg width="28" height="28" viewBox="0 0 28 28" fill="none" aria-hidden="true">
      <circle cx="8"  cy="14" r="5" stroke={C.maroon} strokeWidth="1.8" fill={C.maroonLight} />
      <circle cx="20" cy="14" r="5" stroke={C.maroon} strokeWidth="1.8" fill={C.maroonLight} />
      <line x1="13" y1="14" x2="15" y2="14" stroke={C.maroon} strokeWidth="2" strokeLinecap="round" />
    </svg>
  );
}

function NetworkIcon() {
  return (
    <svg width="28" height="28" viewBox="0 0 28 28" fill="none" aria-hidden="true">
      <circle cx="14" cy="14" r="3.5" fill={C.maroon} />
      <circle cx="5"  cy="6"  r="2.5" fill={C.maroon} opacity="0.5" />
      <circle cx="23" cy="6"  r="2.5" fill={C.maroon} opacity="0.5" />
      <circle cx="5"  cy="22" r="2.5" fill={C.maroon} opacity="0.5" />
      <circle cx="23" cy="22" r="2.5" fill={C.maroon} opacity="0.5" />
      <line x1="14" y1="14" x2="5"  y2="6"  stroke={C.maroon} strokeWidth="1.4" opacity="0.6" />
      <line x1="14" y1="14" x2="23" y2="6"  stroke={C.maroon} strokeWidth="1.4" opacity="0.6" />
      <line x1="14" y1="14" x2="5"  y2="22" stroke={C.maroon} strokeWidth="1.4" opacity="0.6" />
      <line x1="14" y1="14" x2="23" y2="22" stroke={C.maroon} strokeWidth="1.4" opacity="0.6" />
    </svg>
  );
}

function CrossIcon() {
  return (
    <svg width="28" height="28" viewBox="0 0 28 28" fill="none" aria-hidden="true">
      <circle cx="6"  cy="10" r="2.5" fill={C.maroon} opacity="0.55" />
      <circle cx="6"  cy="18" r="2.5" fill={C.maroon} opacity="0.55" />
      <circle cx="22" cy="10" r="2.5" fill={C.maroon} opacity="0.55" />
      <circle cx="22" cy="18" r="2.5" fill={C.maroon} opacity="0.55" />
      <line x1="6"  y1="10" x2="22" y2="18" stroke={C.maroon} strokeWidth="1.5" />
      <line x1="6"  y1="18" x2="22" y2="10" stroke={C.maroon} strokeWidth="1.5" />
    </svg>
  );
}

function ORAIcon() {
  return (
    <svg width="28" height="28" viewBox="0 0 28 28" fill="none" aria-hidden="true">
      {[22, 17, 12, 7].map((w, i) => (
        <rect key={i} x={4} y={5 + i * 5.5} width={w} height={4} rx={1.5}
          fill={C.maroon} opacity={1 - i * 0.2} />
      ))}
    </svg>
  );
}

function TopologyIcon() {
  return (
    <svg width="28" height="28" viewBox="0 0 28 28" fill="none" aria-hidden="true">
      <circle cx="14" cy="5"  r="3" fill={C.maroon} />
      <circle cx="5"  cy="20" r="3" fill={C.maroon} opacity="0.55" />
      <circle cx="23" cy="20" r="3" fill={C.maroon} opacity="0.55" />
      <line x1="14" y1="8"  x2="5"  y2="17" stroke={C.maroon} strokeWidth="1.5" />
      <line x1="14" y1="8"  x2="23" y2="17" stroke={C.maroon} strokeWidth="1.5" />
      <line x1="5"  y1="20" x2="23" y2="20" stroke={C.maroon} strokeWidth="1.5" opacity="0.5" />
    </svg>
  );
}

function LipidIcon() {
  return (
    <svg width="28" height="28" viewBox="0 0 28 28" fill="none" aria-hidden="true">
      <ellipse cx="14" cy="14" rx="10" ry="6" stroke={C.maroon} strokeWidth="1.8" fill={C.maroonLight} />
      <ellipse cx="14" cy="14" rx="10" ry="6" stroke={C.maroon} strokeWidth="1.8" fill="none"
        transform="rotate(60 14 14)" opacity="0.5" />
      <ellipse cx="14" cy="14" rx="10" ry="6" stroke={C.maroon} strokeWidth="1.8" fill="none"
        transform="rotate(120 14 14)" opacity="0.3" />
    </svg>
  );
}

function StatsIcon() {
  return (
    <svg width="28" height="28" viewBox="0 0 28 28" fill="none" aria-hidden="true">
      {/* Volcano plot silhouette */}
      <circle cx="10" cy="8"  r="2" fill={C.maroon} />
      <circle cx="18" cy="7"  r="2" fill={C.maroon} />
      <circle cx="7"  cy="18" r="1.5" fill={C.maroon} opacity="0.4" />
      <circle cx="14" cy="20" r="1.5" fill={C.maroon} opacity="0.4" />
      <circle cx="21" cy="17" r="1.5" fill={C.maroon} opacity="0.4" />
      <line x1="4" y1="23" x2="24" y2="23" stroke={C.maroon} strokeWidth="1.5" opacity="0.3" />
      <line x1="14" y1="4"  x2="14" y2="23" stroke={C.maroon} strokeWidth="1.5" opacity="0.3" strokeDasharray="2 2" />
    </svg>
  );
}

// ─── Layout styles ─────────────────────────────────────────────────────────
const heroSection = {
  background: C.white,
  borderBottom: `1px solid ${C.border}`,
  padding: '72px 24px 64px',
};

const heroInner = {
  maxWidth: 640,
  margin: '0 auto',
  textAlign: 'center',
  display: 'flex',
  flexDirection: 'column',
  alignItems: 'center',
  gap: 20,
};

const logoArea = {
  marginBottom: 4,
};

const heroHeading = {
  fontSize: 26,
  fontWeight: 700,
  color: C.textPrimary,
  lineHeight: 1.4,
  margin: 0,
  letterSpacing: '-0.01em',
};

const heroBody = {
  fontSize: 15,
  color: C.textSecondary,
  lineHeight: 1.65,
  margin: 0,
  maxWidth: 500,
};

const moduleSection = {
  padding: '56px 24px 64px',
};

const sectionInner = {
  maxWidth: 1040,
  margin: '0 auto',
};

const sectionHeader = {
  marginBottom: 40,
};

const sectionTitle = {
  fontSize: 20,
  fontWeight: 700,
  color: C.textPrimary,
  margin: '0 0 8px',
  letterSpacing: '-0.01em',
};

const sectionSub = {
  fontSize: 14,
  color: C.textSecondary,
  margin: 0,
  lineHeight: 1.6,
};

const groupBlock = {
  marginBottom: 36,
};

const groupLabelRow = {
  display: 'flex',
  alignItems: 'center',
  gap: 12,
  marginBottom: 16,
};

const groupLabelText = {
  fontSize: 11,
  fontWeight: 700,
  textTransform: 'uppercase',
  letterSpacing: '0.08em',
  color: C.maroon,
  whiteSpace: 'nowrap',
};

const groupRule = {
  flex: 1,
  height: 1,
  background: C.border,
};

const tilesGrid = {
  display: 'flex',
  flexWrap: 'wrap',
  gap: 14,
};

const tileBase = {
  position: 'relative',
  width: 190,
  minHeight: 108,
  background: C.bgCard,
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
};

const tileIconWrap = {
  flexShrink: 0,
};

const tileTitleText = {
  fontSize: 13,
  fontWeight: 700,
  color: C.textPrimary,
  lineHeight: 1.3,
};

const tileSubText = {
  fontSize: 11,
  color: C.textMuted,
  lineHeight: 1.3,
};

const overlayBase = {
  position: 'absolute',
  inset: 0,
  background: 'rgba(60, 10, 20, 0.91)',
  padding: '14px 16px',
  display: 'flex',
  flexDirection: 'column',
  justifyContent: 'center',
  gap: 10,
  transition: 'opacity 0.18s ease',
  borderRadius: 7,
};

const overlayText = {
  fontSize: 12,
  color: '#f0e8ea',
  margin: 0,
  lineHeight: 1.5,
};

const overlayAction = {
  fontSize: 11,
  color: '#e8a0aa',
  fontWeight: 600,
};

const footerStyle = {
  borderTop: `1px solid ${C.border}`,
  background: C.white,
  padding: '18px 24px',
  textAlign: 'center',
};

const footerText = {
  fontSize: 12,
  color: C.textMuted,
  letterSpacing: '0.02em',
};
