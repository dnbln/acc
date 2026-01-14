export type ExampleSource = {
  id: string;
  label: string;
  code: string;
  optimizations: string[];
};

export const examples: ExampleSource[] = [
  {
    id: "available_expressions",
    label: "Available Expressions",
    optimizations: ["vips", "hp"],
    code: `int f(int a, int b, int c){
    int x=0;
    int y=0;
    int z;
    int s= b+c;
    if(a+b<0) {
        x=s;
    }
    if (a+b>0){
        y=c+b;
        b=2;
    }
    z = b+c;
    return z;
}
`,
  },
  {
    id: "busy_expressions",
    label: "Busy Expressions",
    optimizations: ["hp"],
    code: `int foo(int a, int b, int t, int u){
    int x=a+b;
    int y=a*b;
    if(t<u) {
        x=a-b;
    }else{
        y=a-b;
    }
    x=x*y;
    return x;
}
`,
  },
  {
    id: "dead_values",
    label: "Dead Values",
    optimizations: ["cpdvetdb"],
    code: `int foo(int a, int b){
    int x = a + b;
    int y = a - b;
    int z = a * b;
    int w = a / b;

    if (a > b) {
        x = x + 1;
    } else {
        y = y - 1;
    }

    return x;
}
`,
  },
  {
    id: "reaching_definitions",
    label: "Reaching Definitions",
    optimizations: [],
    code: `int foo(int a, int b){
    int x;
    int y;
    if(a<0) {
        x=1;
    } else {
        if (a>0) {
            x=2;
        }
    }
    if(b>0) {
        y=x;
    } else {
        y=0;
    }
    return y;
}
`,
  },
];
