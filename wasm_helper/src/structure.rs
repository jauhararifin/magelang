use std::rc::Rc;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum NumType {
    I32,
    I64,
    F32,
    F64,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum RefType {
    FuncRef,
    ExternRef,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum ValType {
    Num(NumType),
    Ref(RefType),
}

pub type ResultType = Vec<ValType>;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct FuncType {
    pub parameters: ResultType,
    pub returns: ResultType,
}

#[derive(Debug, Clone)]
pub struct Limits {
    pub min: u32,
    pub max: Option<u32>,
}

pub type MemType = Limits;

#[derive(Debug, Clone)]
pub struct TableType {
    pub limits: Limits,
    pub ref_type: RefType,
}

#[derive(Debug)]
pub enum Mut {
    Const,
    Var,
}

#[derive(Debug)]
pub struct GlobalType {
    pub mutability: Mut,
    pub ty: ValType,
}

#[derive(Debug)]
pub enum ExternType {
    Func(FuncType),
    Table(TableType),
    Mem(MemType),
    Global(GlobalType),
}

#[derive(Debug)]
pub struct Expr(pub Vec<Instr>);

#[derive(Debug)]
pub enum Instr {
    // inn.const unn
    I32Const(i32),
    I64Const(i64),

    // fnn.const fnn
    F32Const(f32),
    F64Const(f64),

    // inn.iunop
    I32Clz,
    I32Ctz,
    I32Popcnt,
    I64Clz,
    I64Ctz,
    I64Popcnt,

    // fnn.funop
    F32Abs,
    F32Neg,
    F32Sqrt,
    F32Ceil,
    F32Floor,
    F32Trunc,
    F32Nearest,
    F64Abs,
    F64Neg,
    F64Sqrt,
    F64Ceil,
    F64Floor,
    F64Trunc,
    F64Nearest,

    // inn.ibinop
    I32Add,
    I64Add,
    I32Sub,
    I64Sub,
    I32Mul,
    I64Mul,
    I32DivU,
    I64DivU,
    I32DivS,
    I64DivS,
    I32RemU,
    I64RemU,
    I32RemS,
    I64RemS,
    I32And,
    I64And,
    I32Or,
    I64Or,
    I32Xor,
    I64Xor,
    I32Shl,
    I64Shl,
    I32ShrU,
    I64ShrU,
    I32ShrS,
    I64ShrS,
    I32Rotl,
    I64Rotl,
    I32Rotr,
    I64Rotr,

    // fnn.fbinop
    F32Add,
    F64Add,
    F32Sub,
    F64Sub,
    F32Mul,
    F64Mul,
    F32Div,
    F64Div,
    F32Min,
    F64Min,
    F32Max,
    F64Max,
    F32CopySign,
    F64CopySign,

    // inn.itestop
    I32Eqz,
    I64Eqz,

    // inn.irelop
    I32Eq,
    I64Eq,
    I32Ne,
    I64Ne,
    I32LtU,
    I64LtU,
    I32LtS,
    I64LtS,
    I32GtU,
    I64GtU,
    I32GtS,
    I64GtS,
    I32LeU,
    I64LeU,
    I32LeS,
    I64LeS,
    I32GeU,
    I64GeU,
    I32GeS,
    I64GeS,

    // fnn.frelop
    F32Eq,
    F64Eq,
    F32Ne,
    F64Ne,
    F32Lt,
    F64Lt,
    F32Gt,
    F64Gt,
    F32Le,
    F64Le,
    F32Ge,
    F64Ge,

    // inn.extend8_s
    I32Extend8S,
    I64Extend8S,

    // inn.extend16_s
    I32Extend16S,
    I64Extend16S,

    I64Extend32S,
    I32WrapI64,
    I64ExtendI32U,
    I64ExtendI32S,

    // inn.trunc_fmm_sx
    I32TruncF32U,
    I32TruncF32S,
    I32TruncF64U,
    I32TruncF64S,
    I64TruncF32U,
    I64TruncF32S,
    I64TruncF64U,
    I64TruncF64S,

    // inn.trunc_sat_fmm_sx
    I32TruncSatF32U,
    I32TruncSatF32S,
    I32TruncSatF64U,
    I32TruncSatF64S,
    I64TruncSatF32U,
    I64TruncSatF32S,
    I64TruncSatF64U,
    I64TruncSatF64S,

    F32DemoteF64,
    F64PromoteF32,

    // fnn.convert_imm_sx
    F32ConvertI32U,
    F32ConvertI32S,
    F32ConvertI64U,
    F32ConvertI64S,
    F64ConvertI32U,
    F64ConvertI32S,
    F64ConvertI64U,
    F64ConvertI64S,

    // inn.reinterpret_fnn
    I32ReinterpretF32,
    I64ReinterpretF64,

    // fnn.reinterpret_inn
    F32ReinterpretI32,
    F64ReinterpretI64,

    // TODO: support vector instruction
    RefNull(RefType),
    RefIsNull,
    RefFunc(FuncIdx),

    Drop,
    // TODO: support select instruction
    LocalGet(LocalIdx),
    LocalSet(LocalIdx),
    LocalTee(LocalIdx),
    GlobalGet(GlobalIdx),
    GlobalSet(GlobalIdx),

    TableGet(TableIdx),
    TableSet(TableIdx),
    TableSize(TableIdx),
    TableGrow(TableIdx),
    TableFill(TableIdx),
    TableCopy(TableIdx, TableIdx),
    TableInit(TableIdx, ElemIdx),
    ElemDrop(ElemIdx),

    // inn.load memarg
    I32Load(MemArg),
    I64Load(MemArg),

    // fnn.load memarg
    F32Load(MemArg),
    F64Load(MemArg),

    // inn.store memarg
    I32Store(MemArg),
    I64Store(MemArg),

    // fnn.store memarg
    F32Store(MemArg),
    F64Store(MemArg),

    // inn.load8_sx memarg
    I32Load8U(MemArg),
    I32Load8S(MemArg),
    I64Load8U(MemArg),
    I64Load8S(MemArg),

    // inn.load16_sx memarg
    I32Load16U(MemArg),
    I32Load16S(MemArg),
    I64Load16U(MemArg),
    I64Load16S(MemArg),

    // i64.load32_sx memarg
    I64Load32U(MemArg),
    I64Load32S(MemArg),

    // inn.store8 memarg
    I32Store8(MemArg),
    I64Store8(MemArg),

    // inn.store16 memarg
    I32Store16(MemArg),
    I64Store16(MemArg),

    I64Store32(MemArg),

    // TODO: support vector related memory access
    MemorySize,
    MemoryGrow,
    MemoryFill,
    MemoryCopy,
    MemoryInit(DataIdx),
    DataDrop(DataIdx),

    Nop,
    Unreachable,
    Block(BlockType, Vec<Instr>),
    Loop(BlockType, Vec<Instr>),
    If(BlockType, Vec<Instr>, Vec<Instr>),
    Br(LabelIdx),
    BrIf(LabelIdx),
    BrTable(Vec<LabelIdx>, LabelIdx),
    Return,
    Call(FuncIdx),
    CallIndirect(TableIdx, TypeIdx),
}

#[derive(Debug)]
pub struct MemArg {
    pub offset: u32,
    pub align: u32,
}

#[derive(Debug)]
pub enum BlockType {
    None,
    Ty(TypeIdx),
    ValTy(ValType),
}

#[derive(Debug)]
pub struct Module {
    pub types: Vec<FuncType>,
    pub funcs: Vec<Func>,
    pub tables: Vec<Table>,
    pub mems: Vec<Mem>,
    pub globals: Vec<Global>,
    pub elems: Vec<Elem>,
    pub datas: Vec<Data>,
    pub start: Option<FuncIdx>,
    pub imports: Vec<Import>,
    pub exports: Vec<Export>,
}

pub type TypeIdx = u32;
pub type FuncIdx = u32;
pub type TableIdx = u32;
pub type MemIdx = u32;
pub type GlobalIdx = u32;
pub type ElemIdx = u32;
pub type DataIdx = u32;
pub type LocalIdx = u32;
pub type LabelIdx = u32;

#[derive(Debug)]
pub struct Func {
    pub name: String,
    pub ty: TypeIdx,
    pub locals: Vec<Local>,
    pub body: Expr,
}

#[derive(Debug)]
pub struct Local {
    pub name: String,
    pub ty: ValType,
}

pub type Table = TableType;
pub type Mem = MemType;

#[derive(Debug)]
pub struct Global {
    pub ty: GlobalType,
    pub init: Expr,
}

#[derive(Debug)]
pub struct Elem {
    pub ty: RefType,
    pub init: Vec<Expr>,
    pub mode: ElemMode,
}

#[derive(Debug)]
pub enum ElemMode {
    Passive,
    Active { table: TableIdx, offset: Expr },
    Declarative,
}

#[derive(Debug)]
pub struct Bytes(pub Rc<[u8]>);

impl From<&str> for Bytes {
    fn from(value: &str) -> Self {
        Self(value.as_bytes().into())
    }
}

#[derive(Debug)]
pub struct Data {
    pub init: Bytes,
    pub mode: DataMode,
}

#[derive(Debug)]
pub enum DataMode {
    Passive,
    Active { memory: MemIdx, offset: Expr },
}

#[derive(Debug)]
pub struct Export {
    pub name: String,
    pub desc: ExportDesc,
}

#[derive(Debug)]
pub enum ExportDesc {
    Func(FuncIdx),
    Table(TableType),
    Mem(MemIdx),
    Global(GlobalType),
}

#[derive(Debug)]
pub struct Import {
    pub module: String,
    pub name: String,
    pub desc: ImportDesc,
}

#[derive(Debug)]
pub enum ImportDesc {
    Func(TypeIdx),
    Table(TableType),
    Mem(MemType),
    Global(GlobalType),
}
