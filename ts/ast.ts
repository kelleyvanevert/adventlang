export type AstNode = {
  id: number;
  span: [number, number];
};

export type TypeVar = AstNode & {
  kind: "typevar";
  id: string;
};

export type TypeFn = AstNode & {
  kind: "fn";
  generics?: TypeVar[];
  args?: Type[];
  ret?: Type;
};

export type TypePrimitive = AstNode & {
  kind: "nil" | "bool" | "str" | "int" | "float" | "num" | "regex";
};

export type Type =
  | TypeVar
  | TypePrimitive
  | TypeFn
  | (AstNode & {
      kind: "list";
      el?: Type;
    })
  | (AstNode & {
      kind: "tuple";
      els?: Type[];
    })
  | (AstNode & {
      kind: "dict";
      el?: { key: Type; val: Type };
    })
  | (AstNode & {
      kind: "nullable";
      child: Type;
    });

declare const t: Type;
