import { useEffect, useMemo, useRef, useState, type MouseEvent } from "react";
import SplitPane from "split-pane-react";
import "split-pane-react/esm/themes/default.css";
import CodeMirror from "@uiw/react-codemirror";
import { cpp } from "@codemirror/lang-cpp";
import { lintGutter, linter, type Diagnostic } from "@codemirror/lint";
import Select from "react-select";
import * as d3 from "d3";
import { Graphviz } from "graphviz-react";
import { compile } from "./wasm";
import { examples } from "./examples";
import "./index.css";

type CompilerDiagnostic = {
  range: { start: number; end: number } | null;
  error_name: string;
  severity: "error" | "warning";
};

type CompileResult = {
  ok: boolean;
  ast: unknown | null;
  ast_text?: string | null;
  cfg_text: string | null;
  graphviz: string | null;
  errors: CompilerDiagnostic[];
  warnings: CompilerDiagnostic[];
};

type ViewKind = "ir" | "ast" | "graphviz";

type ViewOption = {
  value: ViewKind;
  label: string;
  isDisabled?: boolean;
};

function IRView({ cfgText }: { cfgText: string | null }) {
  return (
    <pre className="h-full overflow-auto whitespace-pre-wrap p-4 font-mono text-sm text-slate-200">
      {cfgText}
    </pre>
  );
}

type TreeNode = {
  label: string;
  type?: string;
  children?: TreeNode[];
};

const MAX_LABEL_LENGTH = 48;

const formatLabel = (value: string) => {
  if (value.length <= MAX_LABEL_LENGTH) return value;
  return `${value.slice(0, MAX_LABEL_LENGTH - 3)}...`;
};

const buildChildren = (value: unknown): TreeNode[] => {
  if (value === null || value === undefined || typeof value !== "object") {
    return [buildTree(value, "value")];
  }
  if (Array.isArray(value)) {
    return value.map((item, index) => buildTree(item, `#${index}`));
  }
  const record = value as Record<string, unknown>;
  if ("node" in record && "span" in record) {
    return buildChildren(record.node);
  }
  return Object.keys(record)
    .filter(key => key !== "span")
    .map(key => buildTree(record[key], key));
};

const buildTree = (value: unknown, label: string): TreeNode => {
  if (value === null) {
    return { label: `${label}: null` };
  }
  if (value === undefined) {
    return { label: `${label}: undefined` };
  }
  if (typeof value === "string") {
    return { label: formatLabel(`${label}: "${value}"`) };
  }
  if (typeof value === "number" || typeof value === "boolean") {
    return { label: `${label}: ${String(value)}` };
  }
  if (Array.isArray(value)) {
    return {
      label: `${label} [${value.length}]`,
      children: value.map((item, index) => buildTree(item, `#${index}`)),
    };
  }
  if (typeof value === "object") {
    const record = value as Record<string, unknown>;
    if ("node" in record && "span" in record) {
      return buildTree(record.node, label);
    }
    const keys = Object.keys(record);
    if (keys.length === 1) {
      const key = keys[0] ?? label;
      return {
        label,
        type: key,
        children: buildChildren(record[key]),
      };
    }
    return {
      label,
      children: buildChildren(record),
    };
  }
  return { label: `${label}: ${String(value)}` };
};

function ASTView({ astData }: { astData: unknown | null }) {
  const svgRef = useRef<SVGSVGElement>(null);

  const parsedAst = useMemo(() => {
    if (!astData) return null;
    if (typeof astData === "string") {
      try {
        return JSON.parse(astData) as unknown;
      } catch {
        return astData;
      }
    }
    return astData;
  }, [astData]);

  const treeData = useMemo(() => {
    if (!parsedAst) return null;
    return buildTree(parsedAst, "AST");
  }, [parsedAst]);

  useEffect(() => {
    if (!treeData || !svgRef.current) return;

    const svg = d3.select(svgRef.current);
    svg.selectAll("*").remove();

    const root = d3.hierarchy(treeData);
    const treeLayout = d3.tree<TreeNode>().nodeSize([200, 44]);
    treeLayout(root);

    const nodes = root.descendants();
    const minX = d3.min(nodes, node => node.x) ?? 0;
    const maxX = d3.max(nodes, node => node.x) ?? 0;
    const maxY = d3.max(nodes, node => node.y) ?? 0;
    const width = maxX - minX + 160;
    const height = maxY + 160;

    svg
      .attr("width", width)
      .attr("height", height)
      .attr("viewBox", `0 0 ${width} ${height}`)
      .attr("style", "touch-action: none;");

    const g = svg.append("g");
    const baseTransform = d3.zoomIdentity.translate(64 - minX, 48);

    const linkGenerator = d3
      .linkVertical<d3.HierarchyPointLink<TreeNode>, d3.HierarchyPointNode<TreeNode>>()
      .x(node => node.x)
      .y(node => node.y);

    g.selectAll("path")
      .data(root.links())
      .enter()
      .append("path")
      .attr("d", linkGenerator)
      .attr("fill", "none")
      .attr("stroke", "rgba(148, 163, 184, 0.35)")
      .attr("stroke-width", 1);

    const nodeGroup = g
      .selectAll("g")
      .data(nodes)
      .enter()
      .append("g")
      .attr("transform", node => `translate(${node.x},${node.y})`);

    nodeGroup
      .append("circle")
      .attr("r", 9)
      .attr("fill", "#38bdf8")
      .attr("stroke", "rgba(15, 23, 42, 0.8)")
      .attr("stroke-width", 1);

    nodeGroup
      .append("text")
      .text(node => node.data.label)
      .attr("x", 14)
      .attr("dy", "0.32em")
      .attr("fill", "#e2e8f0")
      .attr("font-family", "ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace")
      .attr("font-size", 12);

    nodeGroup
      .append("title")
      .text(node => node.data.type ?? "");

    const zoom = d3
      .zoom<SVGSVGElement, unknown>()
      .scaleExtent([0.4, 3])
      .on("zoom", event => {
        g.attr("transform", event.transform.toString());
      });

    svg.call(zoom);
    svg.call(zoom.transform, baseTransform);
  }, [treeData]);

  if (!astData) {
    return (
      <div className="h-full overflow-auto p-4 font-mono text-sm text-slate-400">
        AST output is not available yet.
      </div>
    );
  }

  return (
    <div className="h-full w-full overflow-auto" onWheel={event => event.stopPropagation()}>
      <svg ref={svgRef} className="block" />
    </div>
  );
}

function GraphVizView({ graphviz }: { graphviz: string | null }) {
  if (!graphviz) {
    return (
      <pre className="h-full overflow-auto whitespace-pre-wrap p-4 font-mono text-sm text-slate-200">
        GraphViz output is not available yet.
      </pre>
    );
  }

  const containerRef = useRef<HTMLDivElement | null>(null);
  const dragOrigin = useRef<{ x: number; y: number; left: number; top: number } | null>(null);
  const [scale, setScale] = useState(1);
  const [dragging, setDragging] = useState(false);

  const clampScale = (value: number) => Math.max(0.5, Math.min(2.5, value));

  const zoomIn = () => setScale(value => clampScale(value + 0.1));
  const zoomOut = () => setScale(value => clampScale(value - 0.1));
  const resetZoom = () => setScale(1);

  const onMouseDown = (event: MouseEvent<HTMLDivElement>) => {
    if (event.button !== 0) return;
    const container = containerRef.current;
    if (!container) return;
    dragOrigin.current = {
      x: event.clientX,
      y: event.clientY,
      left: container.scrollLeft,
      top: container.scrollTop,
    };
    setDragging(true);
  };

  const onMouseMove = (event: MouseEvent<HTMLDivElement>) => {
    const container = containerRef.current;
    if (!container || !dragOrigin.current) return;
    event.preventDefault();
    const { x, y, left, top } = dragOrigin.current;
    container.scrollLeft = left - (event.clientX - x);
    container.scrollTop = top - (event.clientY - y);
  };

  const onMouseUp = () => {
    dragOrigin.current = null;
    setDragging(false);
  };

  return (
    <div className="h-full overflow-auto p-4 text-slate-200">
      <div className="graphviz-view flex h-full flex-col rounded-xl border border-white/10 bg-[#0b0d11]">
        <div className="flex items-center justify-end gap-2 border-b border-white/10 px-3 py-2">
          <button
            type="button"
            onClick={zoomOut}
            className="rounded-md border border-white/10 bg-[#101423] px-2 py-1 text-xs text-slate-200 hover:bg-[#1f2937]"
          >
            -
          </button>
          <button
            type="button"
            onClick={resetZoom}
            className="rounded-md border border-white/10 bg-[#101423] px-2 py-1 text-xs text-slate-200 hover:bg-[#1f2937]"
          >
            Reset
          </button>
          <button
            type="button"
            onClick={zoomIn}
            className="rounded-md border border-white/10 bg-[#101423] px-2 py-1 text-xs text-slate-200 hover:bg-[#1f2937]"
          >
            +
          </button>
        </div>
        <div
          ref={containerRef}
          className={`h-full overflow-auto p-4 ${
            dragging ? "cursor-grabbing" : "cursor-grab"
          }`}
          onMouseDown={onMouseDown}
          onMouseMove={onMouseMove}
          onMouseUp={onMouseUp}
          onMouseLeave={onMouseUp}
        >
          <div style={{ transform: `scale(${scale})`, transformOrigin: "top left" }}>
            <Graphviz
              dot={graphviz}
              options={{ zoom: false, fit: true, width: 900, height: 600 }}
            />
          </div>
        </div>
      </div>
    </div>
  );
}

export function App() {
  const [sizes, setSizes] = useState<[number | string, number | string]>([520, "auto"]);
  const [outputSizes, setOutputSizes] = useState<[number | string, number | string]>([
    "auto",
    180,
  ]);
  const [selectedExampleId, setSelectedExampleId] = useState(
    examples[0]?.id ?? ""
  );
  const [code, setCode] = useState(() => examples[0]?.code ?? "");
  const [optimizations, setOptimizations] = useState<string[]>(
    () => examples[0]?.optimizations ?? []
  );
  const [compileResult, setCompileResult] = useState<CompileResult | null>(null);
  const [view, setView] = useState<ViewKind>("ir");

  const diagnostics = useMemo(() => {
    if (!compileResult) return [];
    return [...compileResult.errors, ...compileResult.warnings];
  }, [compileResult]);

  const astData = compileResult?.ast ?? compileResult?.ast_text ?? null;

  const viewOptions = useMemo<ViewOption[]>(() => {
    const hasResult = compileResult !== null;
    const hasAst = astData !== null;
    return [
      { value: "ir", label: "IR", isDisabled: hasResult && !compileResult?.cfg_text },
      { value: "ast", label: "AST", isDisabled: hasResult && !hasAst },
      {
        value: "graphviz",
        label: "GraphViz",
        isDisabled: hasResult && !compileResult?.graphviz,
      },
    ];
  }, [compileResult, astData]);

  useEffect(() => {
    const firstAvailable = viewOptions.find(option => !option.isDisabled);
    if (!firstAvailable) return;
    const current = viewOptions.find(option => option.value === view);
    if (!current || current.isDisabled) {
      setView(firstAvailable.value);
    }
  }, [viewOptions, view]);

  const compileLinter = useMemo(
    () =>
      linter(
        async view => {
          try {
            const source = view.state.doc.toString();
            const result = await compile(source, optimizations, true);
            if (!result) {
              setCompileResult(null);
              return [];
            }
            setCompileResult(result as CompileResult);
            const docLength = view.state.doc.length;
            const diagnostics: Diagnostic[] = [];
            const addDiagnostic = (item: {
              range: { start: number; end: number } | null;
              error_name: string;
              severity: "error" | "warning";
            }) => {
              let from = 0;
              let to = 0;
              if (item.range) {
                from = Math.max(0, Math.min(docLength, item.range.start));
                to = Math.max(from, Math.min(docLength, item.range.end));
              }
              diagnostics.push({
                from,
                to,
                severity: item.severity,
                message: item.error_name,
              });
            };
            result.errors.forEach(addDiagnostic);
            result.warnings.forEach(addDiagnostic);
            return diagnostics;
          } catch (error) {
            setCompileResult(null);
            return [];
          }
        },
        { delay: 300 }
      ),
    [optimizations]
  );

  const statusColor = useMemo(() => {
    if (!compileResult) return "bg-slate-500";
    if (compileResult.errors.length > 0) return "bg-red-500";
    if (compileResult.warnings.length > 0) return "bg-amber-400";
    if (compileResult.ok) return "bg-emerald-400";
    return "bg-slate-500";
  }, [compileResult]);

  const selectedOption = viewOptions.find(option => option.value === view) ?? viewOptions[0];

  const exampleOptions = useMemo(
    () => examples.map(example => ({ value: example.id, label: example.label })),
    []
  );

  const optimizationOptions = useMemo(
    () => [
      {
        value: "cp",
        description: "constant propagation; folds constants and propagates constant values through the CFG.",
      },
      {
        value: "dve",
        description: "dead value elimination; removes assignments to values that are not live.",
      },
      {
        value: "cpdvetdb",
        description:
          "loop of constant propagation, dead value elimination, and trim-dead-blocks until no changes.",
      },
      {
        value: "bi",
        description:
          "block inliner; inlines a block into its predecessor when it has a single predecessor (and a few structured branch cases).",
      },
      {
        value: "hp",
        description:
          "hoist pass; hoists repeated pure expressions to a common dominating block when safe.",
      },
      {
        value: "ps",
        description: "phi simplification; replaces phi nodes with identical sources by simple assignments.",
      },
      {
        value: "vi",
        description:
          "value inliner; replaces uses of values assigned from other values, chasing equality chains.",
      },
      {
        value: "vips",
        description: "loop of phi simplification and value inliner until no changes.",
      },
      {
        value: "phi2sel",
        description:
          "converts certain two-predecessor phi nodes into a select instruction under a matching branch pattern.",
      },
      {
        value: "bd",
        description:
          "block deduplication; merges empty blocks with identical tails (when safe with phi nodes).",
      },
      {
        value: "tu",
        description:
          "tail unification; replaces conditional branches that go to the same target with an unconditional branch.",
      },
      {
        value: "tdb",
        description: "trim dead blocks; removes unreachable blocks and relinks the CFG.",
      },
    ],
    []
  );

  const selectedExample =
    exampleOptions.find(option => option.value === selectedExampleId) ??
    exampleOptions[0] ??
    null;
  const selectedOptimizations = optimizationOptions.filter(option =>
    optimizations.includes(option.value)
  );

  return (
    <div className="h-screen w-screen bg-[#0f1115] text-slate-100">
      <div className="flex flex-wrap items-center gap-4 border-b border-white/10 bg-[#0b0d11] px-4 py-3 text-xs uppercase tracking-[0.2em] text-slate-400">
        <div className="mr-auto text-sm tracking-[0.3em] text-slate-200">
          Compiler Explorer
        </div>
        <div className="min-w-[320px] flex-1 max-w-[640px]">
          <Select
            classNamePrefix="cm-select"
            options={optimizationOptions}
            value={selectedOptimizations}
            placeholder="Optimizations"
            onChange={options =>
              setOptimizations((options ?? []).map(option => option.value))
            }
            isMulti
            closeMenuOnSelect={false}
            getOptionLabel={option => option.value}
            getOptionValue={option => option.value}
            formatOptionLabel={(option, { context }) =>
              context === "menu"
                ? `${option.value} — ${option.description}`
                : option.value
            }
            styles={{
              control: base => ({
                ...base,
                backgroundColor: "transparent",
                border: "1px solid rgba(148, 163, 184, 0.2)",
                minHeight: 36,
                boxShadow: "none",
              }),
              indicatorsContainer: base => ({
                ...base,
                height: 36,
              }),
              input: base => ({
                ...base,
                margin: 0,
                padding: 0,
              }),
              menu: base => ({
                ...base,
                backgroundColor: "#0f1115",
                border: "1px solid rgba(148, 163, 184, 0.2)",
              }),
              multiValue: base => ({
                ...base,
                backgroundColor: "rgba(148, 163, 184, 0.2)",
                marginRight: 6,
              }),
              multiValueLabel: base => ({
                ...base,
                color: "#e2e8f0",
                letterSpacing: "0.2em",
              }),
              option: (base, state) => ({
                ...base,
                backgroundColor: state.isFocused ? "#1f2937" : "transparent",
                color: state.isDisabled ? "#475569" : "#e2e8f0",
                cursor: state.isDisabled ? "not-allowed" : "pointer",
                letterSpacing: "0.2em",
              }),
              indicatorSeparator: base => ({ ...base, display: "none" }),
              dropdownIndicator: base => ({
                ...base,
                color: "#94a3b8",
                padding: "0 8px",
              }),
            }}
          />
        </div>
      </div>
      <SplitPane
        split="vertical"
        sizes={sizes}
        onChange={setSizes}
        minSize={240}
        className="h-[calc(100%-60px)]"
      >
        <div className="h-full border-r border-white/10 bg-[#0b0d11]">
          <div className="flex items-center justify-between gap-3 border-b border-white/10 px-4 py-2 text-xs uppercase tracking-[0.2em] text-slate-400 min-w-[540px]">
            <div>Editor</div>
            <div className="min-w-[220px]">
              <Select
                classNamePrefix="cm-select"
                options={exampleOptions}
                value={selectedExample}
                onChange={option => {
                  if (!option) return;
                  setSelectedExampleId(option.value);
                  const example = examples.find(item => item.id === option.value);
                  if (example) {
                    setCode(example.code);
                    setOptimizations(example.optimizations);
                  }
                }}
                isSearchable={false}
                styles={{
                  control: base => ({
                    ...base,
                    backgroundColor: "transparent",
                    border: "1px solid rgba(148, 163, 184, 0.2)",
                    minHeight: 30,
                    boxShadow: "none",
                  }),
                  indicatorsContainer: base => ({
                    ...base,
                  }),
                  input: base => ({
                    ...base,
                    margin: 0,
                    padding: 0,
                  }),
                  menu: base => ({
                    ...base,
                    backgroundColor: "#0f1115",
                    border: "1px solid rgba(148, 163, 184, 0.2)",
                  }),
                  singleValue: base => ({
                    ...base,
                    color: "#cbd5f5",
                    letterSpacing: "0.2em",
                  }),
                  option: (base, state) => ({
                    ...base,
                    backgroundColor: state.isFocused ? "#1f2937" : "transparent",
                    color: state.isDisabled ? "#475569" : "#e2e8f0",
                    cursor: state.isDisabled ? "not-allowed" : "pointer",
                    letterSpacing: "0.2em",
                  }),
                  indicatorSeparator: base => ({ ...base, display: "none" }),
                  dropdownIndicator: base => ({
                    ...base,
                    color: "#94a3b8",
                    padding: "0 8px",
                  }),
                }}
              />
            </div>
          </div>
          <CodeMirror
            value={code}
            height="100%"
            theme="dark"
            extensions={[cpp(), lintGutter(), compileLinter]}
            onChange={setCode}
            className="h-[calc(100%-33px)]"
          />
        </div>
        <div className="h-full bg-[#141824]">
          <div className="flex items-center gap-3 border-b border-white/10 px-4 py-2 text-xs uppercase tracking-[0.2em] text-slate-400">
            <span className={`h-2.5 w-2.5 rounded-full ${statusColor}`} />
            <div className="min-w-[180px]">
              <Select
                classNamePrefix="cm-select"
                options={viewOptions}
                value={selectedOption}
                onChange={option => option && setView(option.value)}
                isSearchable={false}
                styles={{
                  control: base => ({
                    ...base,
                    backgroundColor: "transparent",
                    border: "1px solid rgba(148, 163, 184, 0.2)",
                    minHeight: 30,
                    boxShadow: "none",
                  }),
                  menu: base => ({
                    ...base,
                    backgroundColor: "#0f1115",
                    border: "1px solid rgba(148, 163, 184, 0.2)",
                  }),
                  singleValue: base => ({
                    ...base,
                    color: "#cbd5f5",
                    letterSpacing: "0.2em",
                  }),
                  option: (base, state) => ({
                    ...base,
                    backgroundColor: state.isFocused ? "#1f2937" : "transparent",
                    color: state.isDisabled ? "#475569" : "#e2e8f0",
                    cursor: state.isDisabled ? "not-allowed" : "pointer",
                    letterSpacing: "0.2em",
                  }),
                  indicatorSeparator: base => ({ ...base, display: "none" }),
                  dropdownIndicator: base => ({
                    ...base,
                    color: "#94a3b8",
                    padding: "0 8px",
                  }),
                }}
              />
            </div>
          </div>
          <div className="h-[calc(100%-33px)]">
            {diagnostics.length === 0 ? (
              <div className="h-full">
                {view === "ir" && <IRView cfgText={compileResult?.cfg_text ?? null} />}
                {view === "ast" && <ASTView astData={astData} />}
                {view === "graphviz" && (
                  <GraphVizView graphviz={compileResult?.graphviz ?? null} />
                )}
              </div>
            ) : (
              <SplitPane
                split="horizontal"
                sizes={outputSizes}
                onChange={setOutputSizes}
                minSize={120}
                className="h-full"
              >
                <div className="h-full">
                  {view === "ir" && <IRView cfgText={compileResult?.cfg_text ?? null} />}
                  {view === "ast" && <ASTView astData={astData} />}
                  {view === "graphviz" && (
                    <GraphVizView graphviz={compileResult?.graphviz ?? null} />
                  )}
                </div>
                <div className="h-full border-t border-white/10 bg-[#0b0d11]">
                  <div className="px-4 py-2 text-xs uppercase tracking-[0.2em] text-slate-400">
                    Diagnostics
                  </div>
                  <div className="h-[calc(100%-33px)] overflow-auto px-4 pb-3 text-sm text-slate-200">
                    {diagnostics.map((item, index) => (
                      <div key={`${item.severity}-${index}`} className="py-1">
                        <span
                          className={
                            item.severity === "error"
                              ? "text-red-400"
                              : "text-amber-300"
                          }
                        >
                          [{item.severity}]
                        </span>{" "}
                        {item.error_name}
                      </div>
                    ))}
                  </div>
                </div>
              </SplitPane>
            )}
          </div>
        </div>
      </SplitPane>
    </div>
  );
}

export default App;
