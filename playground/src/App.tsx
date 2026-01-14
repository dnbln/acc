import { useEffect, useMemo, useState } from "react";
import SplitPane from "split-pane-react";
import "split-pane-react/esm/themes/default.css";
import CodeMirror from "@uiw/react-codemirror";
import { cpp } from "@codemirror/lang-cpp";
import { lintGutter, linter, type Diagnostic } from "@codemirror/lint";
import Select from "react-select";
import { Graphviz } from "graphviz-react";
import { compile } from "./wasm";
import "./index.css";

type CompilerDiagnostic = {
  range: { start: number; end: number } | null;
  error_name: string;
  severity: "error" | "warning";
};

type CompileResult = {
  ok: boolean;
  ast_text: string | null;
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
      {cfgText ?? "CFG_TEXT"}
    </pre>
  );
}

function ASTView({ astText }: { astText: string | null }) {
  return (
    <pre className="h-full overflow-auto whitespace-pre-wrap p-4 font-mono text-sm text-slate-200">
      {astText ?? "AST output is not available yet."}
    </pre>
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

  return (
    <div className="h-full overflow-auto p-4 text-slate-200">
      <div className="graphviz-view rounded-xl border border-white/10 bg-[#0b0d11] p-4">
        <Graphviz dot={graphviz} options={{ zoom: false, fit: true }} />
      </div>
    </div>
  );
}

export function App() {
  const [sizes, setSizes] = useState<[number, string]>([520, "auto"]);
  const [code, setCode] = useState(`
int main(int a) {
  main(a);
  return 0;
}
`);
  const [compileResult, setCompileResult] = useState<CompileResult | null>(null);
  const [view, setView] = useState<ViewKind>("ir");

  const diagnostics = useMemo(() => {
    if (!compileResult) return [];
    return [...compileResult.errors, ...compileResult.warnings];
  }, [compileResult]);

  const viewOptions = useMemo<ViewOption[]>(() => {
    const hasResult = compileResult !== null;
    return [
      { value: "ir", label: "IR", isDisabled: hasResult && !compileResult?.cfg_text },
      { value: "ast", label: "AST", isDisabled: hasResult && !compileResult?.ast_text },
      {
        value: "graphviz",
        label: "GraphViz",
        isDisabled: hasResult && !compileResult?.graphviz,
      },
    ];
  }, [compileResult]);

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
            const result = await compile(source, ["full"], true);
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
    []
  );

  const statusColor = useMemo(() => {
    if (!compileResult) return "bg-slate-500";
    if (compileResult.errors.length > 0) return "bg-red-500";
    if (compileResult.warnings.length > 0) return "bg-amber-400";
    if (compileResult.ok) return "bg-emerald-400";
    return "bg-slate-500";
  }, [compileResult]);

  const selectedOption = viewOptions.find(option => option.value === view) ?? viewOptions[0];

  return (
    <div className="h-screen w-screen bg-[#0f1115] text-slate-100">
      <SplitPane
        split="vertical"
        sizes={sizes}
        onChange={setSizes}
        minSize={240}
        className="h-full"
      >
        <div className="h-full border-r border-white/10 bg-[#0b0d11]">
          <div className="flex items-center justify-between border-b border-white/10 px-4 py-2 text-xs uppercase tracking-[0.2em] text-slate-400">
            Editor
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
          <div className="flex h-[calc(100%-33px)] flex-col">
            <div className="flex-1">
              {view === "ir" && <IRView cfgText={compileResult?.cfg_text ?? null} />}
              {view === "ast" && <ASTView astText={compileResult?.ast_text ?? null} />}
              {view === "graphviz" && (
                <GraphVizView graphviz={compileResult?.graphviz ?? null} />
              )}
            </div>
            {diagnostics.length > 0 && (
              <div className="border-t border-white/10 bg-[#0b0d11]">
                <div className="px-4 py-2 text-xs uppercase tracking-[0.2em] text-slate-400">
                  Diagnostics
                </div>
                <div className="max-h-40 overflow-auto px-4 pb-3 text-sm text-slate-200">
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
            )}
          </div>
        </div>
      </SplitPane>
    </div>
  );
}

export default App;
