//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include <map>
#include <vector>

#include "cgen.h"
#include "cgen_gc.h"


std::map<Symbol, Class_> class_map;

// the index of each class in this vector is its tag
std::vector<Class_> cls_ordered;

int label_num = 0;

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

#define is_basic_class(name) ((name) == Object || (name) == IO || \
                              (name) == Str || (name) == Int || (name) == Bool)

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol
       arg,
       arg2,
       Bool,
       concat,
       cool_abort,
       copy,
       Int,
       in_int,
       in_string,
       IO,
       length,
       Main,
       main_meth,
       No_class,
       No_type,
       Object,
       out_int,
       out_string,
       prim_slot,
       self,
       SELF_TYPE,
       Str,
       str_field,
       substr,
       type_name,
       val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}

static char *gc_init_names[] =
    { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
    { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os)
{
    // spim wants comments to start with '#'
    os << "# start of generated code\n";

    initialize_constants();
    CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

    os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
    s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")"
      << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
    s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
    emit_partial_load_address(dest,s);
    b.code_ref(s);
    s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
    emit_partial_load_address(dest,s);
    str->code_ref(s);
    s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
    emit_partial_load_address(dest,s);
    i->code_ref(s);
    s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
    emit_label_ref(l,s);
    s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
    s << BEQZ << source << " ";
    emit_label_ref(label,s);
    s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
    s << BEQ << src1 << " " << src2 << " ";
    emit_label_ref(label,s);
    s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
    s << BNE << src1 << " " << src2 << " ";
    emit_label_ref(label,s);
    s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
    s << BLEQ << src1 << " " << src2 << " ";
    emit_label_ref(label,s);
    s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
    s << BLT << src1 << " " << src2 << " ";
    emit_label_ref(label,s);
    s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
    s << BLT << src1 << " " << imm << " ";
    emit_label_ref(label,s);
    s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
    s << BGT << src1 << " " << imm << " ";
    emit_label_ref(label,s);
    s << endl;
}

static void emit_branch(int l, ostream& s)
{
    s << BRANCH;
    emit_label_ref(l,s);
    s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
    emit_store(reg,0,SP,str);
    emit_addiu(SP,SP,-4,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
    emit_push(ACC, s);
    emit_move(ACC, SP, s); // stack end
    emit_move(A1, ZERO, s); // allocate nothing
    s << JAL << gc_collect_names[cgen_Memmgr] << endl;
    emit_addiu(SP,SP,4,s);
    emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
    if (source != (char*)A1) {
        emit_move(A1, source, s);
    }
    s << JAL << "_gc_check" << endl;
}


///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
    s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
    IntEntryP lensym = inttable.add_int(len);

    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);
    s << LABEL                                                          // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD;

    /***** Add dispatch information for class String ******/
    s << Str << DISPTAB_SUFFIX;

    s << endl;                                                  // dispatch table
    s << WORD;
    lensym->code_ref(s);
    s << endl;                                                  // string length
    emit_string_constant(s,str);                                // ascii string
    s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{
    for (List<StringEntry> *l = tbl; l; l = l->tl()) {
        l->hd()->code_def(s,stringclasstag);
    }
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
    s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);
    s << LABEL                                            // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD;

    /***** Add dispatch information for class Int ******/
    s << Int << DISPTAB_SUFFIX;

    s << endl;                                          // dispatch table
    s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
    for (List<IntEntry> *l = tbl; l; l = l->tl()) {
        l->hd()->code_def(s,intclasstag);
    }
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
    s << BOOLCONST_PREFIX << val;
}

//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);
    s << LABEL                                              // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD;

    /***** Add dispatch information for class Bool ******/
    s << Bool << DISPTAB_SUFFIX;

    s << endl;                                            // dispatch table
    s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
    Symbol main    = idtable.lookup_string(MAINNAME);
    Symbol string  = idtable.lookup_string(STRINGNAME);
    Symbol integer = idtable.lookup_string(INTNAME);
    Symbol boolc   = idtable.lookup_string(BOOLNAME);

    str << "\t.data\n" << ALIGN;
    //
    // The following global names must be defined first.
    //
    str << GLOBAL << CLASSNAMETAB << endl;
    str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
    str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
    str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
    str << GLOBAL; falsebool.code_ref(str);  str << endl;
    str << GLOBAL; truebool.code_ref(str);   str << endl;
    str << GLOBAL << INTTAG << endl;
    str << GLOBAL << BOOLTAG << endl;
    str << GLOBAL << STRINGTAG << endl;

    //
    // We also need to know the tag of the Int, String, and Bool classes
    // during code generation.
    //
    str << INTTAG << LABEL
        << WORD << intclasstag << endl;
    str << BOOLTAG << LABEL
        << WORD << boolclasstag << endl;
    str << STRINGTAG << LABEL
        << WORD << stringclasstag << endl;
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
    str << GLOBAL << HEAP_START << endl
        << HEAP_START << LABEL
        << WORD << 0 << endl
        << "\t.text" << endl
        << GLOBAL;
    emit_init_ref(idtable.add_string("Main"), str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("Int"),str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("String"),str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("Bool"),str);
    str << endl << GLOBAL;
    emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
    str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
    falsebool.code_def(str,boolclasstag);
    truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
    //
    // Generate GC choice constants (pointers to GC functions)
    //
    str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
    str << "_MemMgr_INITIALIZER:" << endl;
    str << WORD << gc_init_names[cgen_Memmgr] << endl;
    str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
    str << "_MemMgr_COLLECTOR:" << endl;
    str << WORD << gc_collect_names[cgen_Memmgr] << endl;
    str << GLOBAL << "_MemMgr_TEST" << endl;
    str << "_MemMgr_TEST:" << endl;
    str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
    //
    // Add constants that are required by the code generator.
    //
    stringtable.add_string("");
    inttable.add_string("0");

    stringtable.code_string_table(str,stringclasstag);
    inttable.code_string_table(str,intclasstag);
    code_bools(boolclasstag);
}

int get_class_tag(Symbol name)
{
    for(std::vector<Class_>::size_type i = 0; i < cls_ordered.size(); i++) {
        if (cls_ordered[i]->get_name() == name) {
            return i;
        }
    }

    return -1;
}

CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{
    enterscope();
    if (cgen_debug) {
        cout << "Building CgenClassTable" << endl;
    }

    install_basic_classes();

    // push the rest of the classes in cls_ordered
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        Class_ cls = classes->nth(i);
        cls_ordered.push_back(cls);
        class_map.insert(std::make_pair(cls->get_name(), cls));
    }

    intclasstag = get_class_tag(Int);
    boolclasstag = get_class_tag(Bool);
    stringclasstag = get_class_tag(Str);

    install_classes(classes);
    build_inheritance_tree();

    code();
    exitscope();
}

void CgenClassTable::install_basic_classes()
{
    // The tree package uses these globals to annotate the classes built below.
    //curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");

    //
    // A few special class names are installed in the lookup table but not
    // the class list.  Thus, these classes exist, but are not part of the
    // inheritance hierarchy.
    // No_class serves as the parent of Object and the other special classes.
    // SELF_TYPE is the self class; it cannot be redefined or inherited.
    // prim_slot is a class known to the code generator.
    //
    addid(No_class,
            new CgenNode(class_(No_class,No_class,nil_Features(),filename),
            Basic,this));
    addid(SELF_TYPE,
            new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
            Basic,this));
    addid(prim_slot,
            new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
            Basic,this));

    //
    // The Object class has no parent class. Its methods are
    //        cool_abort() : Object    aborts the program
    //        type_name() : Str        returns a string representation of class name
    //        copy() : SELF_TYPE       returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.
    //

    Class_ c_obj = class_(Object, No_class, append_Features(
            append_Features(
                single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
                single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
            filename);

    install_class(new CgenNode(c_obj, Basic, this));

    //
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE          writes a string to the output
    //        out_int(Int) : SELF_TYPE               "    an int    "  "     "
    //        in_string() : Str                    reads a string from the input
    //        in_int() : Int                         "   an int     "  "     "
    //

    Class_ c_io = class_(IO, Object, append_Features(
            append_Features(
                append_Features(
                    single_Features(method(out_string, single_Formals(formal(arg, Str)),
                                SELF_TYPE, no_expr())),
                    single_Features(method(out_int, single_Formals(formal(arg, Int)),
                                SELF_TYPE, no_expr()))),
                    single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
                single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
            filename);

    install_class(new CgenNode(c_io, Basic, this));

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer.
    //

    Class_ c_int = class_(Int, Object,
             single_Features(attr(val, prim_slot, no_expr())), filename);

    install_class(new CgenNode(c_int, Basic, this));

    //
    // Bool also has only the "val" slot.
    //

    Class_ c_bool = class_(Bool, Object,
            single_Features(attr(val, prim_slot, no_expr())),filename);

    install_class(new CgenNode(c_bool, Basic, this));

    //
    // The class Str has a number of slots and operations:
    //       val                                  ???
    //       str_field                            the string itself
    //       length() : Int                       length of the string
    //       concat(arg: Str) : Str               string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring
    //

    Class_ c_str = class_(Str, Object,
        append_Features(
            append_Features(
                append_Features(
                    append_Features(
                        single_Features(attr(val, Int, no_expr())),
                        single_Features(attr(str_field, prim_slot, no_expr()))),
                    single_Features(method(length, nil_Formals(), Int, no_expr()))),
                single_Features(method(concat, single_Formals(formal(arg, Str)), Str, no_expr()))),
            single_Features(method(substr,
                append_Formals(single_Formals(formal(arg, Int)),
                    single_Formals(formal(arg2, Int))), Str, no_expr()))),
        filename);

    install_class(new CgenNode(c_str, Basic, this));

    // order of insertion assigns class tag starting from 0
    cls_ordered.push_back(c_obj);
    cls_ordered.push_back(c_io);
    cls_ordered.push_back(c_int);
    cls_ordered.push_back(c_bool);
    cls_ordered.push_back(c_str);

    class_map.insert(std::make_pair(Object, c_obj));
    class_map.insert(std::make_pair(IO, c_io));
    class_map.insert(std::make_pair(Int, c_int));
    class_map.insert(std::make_pair(Bool, c_bool));
    class_map.insert(std::make_pair(Str, c_str));
}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
    Symbol name = nd->get_name();

    if (probe(name)) {
        return;
    }

    // The class name is legal, so add it to the list of classes
    // and the symbol table.
    nds = new List<CgenNode>(nd,nds);
    addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
    for(int i = cs->first(); cs->more(i); i = cs->next(i)) {
        install_class(new CgenNode(cs->nth(i),NotBasic,this));
    }
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
    for(List<CgenNode> *l = nds; l; l = l->tl()) {
        set_relations(l->hd());
    }
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
    CgenNode *parent_node = probe(nd->get_parent());
    nd->set_parentnd(parent_node);
    parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
    children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
    assert(parentnd == NULL);
    assert(p != NULL);
    parentnd = p;
}

void CgenClassTable::code_class_name_tab()
{
    str << CLASSNAMETAB << LABEL;

    for(auto it = cls_ordered.begin(); it != cls_ordered.end(); it++) {
        str << WORD;
        stringtable.lookup_string((*it)->get_name()->get_string())->code_ref(str);
        str << endl;
    }
}

/*
 * Prints a table which at index (class_tag) * 4 contains the tag of the
 * corresponding parent class tag.
 */
void CgenClassTable::code_class_parent_tab()
{
    str << CLASSPARENTTAB << LABEL;
    for(auto it = cls_ordered.begin(); it != cls_ordered.end(); it++) {
        if ((*it)->get_name() == Object) {
            str << WORD << INVALID_CLASSTAG << endl;
        } else {
            str << WORD << get_class_tag((*it)->get_parent()) << endl;
        }
    }
}

void CgenClassTable::code_class_obj_tab()
{
    str << CLASSOBJTAB << LABEL;
    for(auto it = cls_ordered.begin(); it != cls_ordered.end(); it++) {
        str << WORD << (*it)->get_name() << PROTOBJ_SUFFIX << endl;
        str << WORD << (*it)->get_name() << CLASSINIT_SUFFIX << endl;
    }
}

void get_class_attrs_recursively(Class_ cls, std::vector<attr_class *> &attrs) {
    if (cls->get_name() != Object) {
        get_class_attrs_recursively(class_map[cls->get_parent()], attrs);
    }

    Features features = cls->get_features();
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        attr_class *at = dynamic_cast<attr_class *>(features->nth(i));

        if (at) {
            attrs.push_back(at);
        }
    }
}

void get_methods_recursively(Class_ cls, std::vector<std::pair<Class_, method_class *> > &methods)
{
    if (cls->get_name() != Object) {
        get_methods_recursively(class_map[cls->get_parent()], methods);
    }

    Features features = cls->get_features();

    for (int i = features->first(); features->more(i); i = features->next(i)) {
        Feature f = features->nth(i);

        method_class *method = dynamic_cast<method_class *>(f);
        if (!method) {
            continue; // f is an attribute not a method, so skip it
        }

        bool name_already_exists = false;
        for (auto it = methods.begin(); it != methods.end(); it++) {
            if (it->second->get_name() == method->get_name()) {
                name_already_exists = true;
                it->first = cls;
            }
        }

        if (!name_already_exists) {
            methods.push_back(std::make_pair(cls, method));
        }
    }
}

void CgenClassTable::code_dispatch_tables()
{
    for(auto it_c = cls_ordered.begin(); it_c != cls_ordered.end(); it_c++) {
        Class_ cls = *it_c;
        str << cls->get_name() << DISPTAB_SUFFIX << LABEL;
        get_methods_recursively(cls, cls->all_methods);

        for (auto it_m = cls->all_methods.begin(); it_m != cls->all_methods.end(); it_m++) {
            str << WORD << it_m->first->get_name() << "." << it_m->second->get_name() << endl;
        }
    }
}

void CgenClassTable::code_prototypes()
{
    for(std::vector<Class_>::size_type i = 0; i < cls_ordered.size(); i++) {
        Class_ cls = cls_ordered[i];

        get_class_attrs_recursively(cls, cls->all_attrs);

        str << WORD << "-1" << endl;
        str << cls->get_name() << PROTOBJ_SUFFIX << LABEL;
        str << WORD << i << endl; // class tag
        str << WORD << DEFAULT_OBJFIELDS + cls->all_attrs.size() << endl; // object size
        str << WORD << cls->get_name() << DISPTAB_SUFFIX << endl;

        for (auto attr : cls->all_attrs) {
            Symbol type = attr->get_type_decl();
            str << WORD;
            if (type == Int) {
                inttable.lookup_string("0")->code_ref(str);
            } else if (type == Bool) {
                falsebool.code_ref(str);
            } else if (type == Str) {
                stringtable.lookup_string("")->code_ref(str);
            } else {
                str << "0";
            }
            str << endl;
        }
    }
}

void CgenClassTable::code_initializers()
{
    for(std::vector<Class_>::size_type i = 0; i < cls_ordered.size(); i++) {
        Class_ cls = cls_ordered[i];

        str << cls->get_name() << CLASSINIT_SUFFIX << LABEL;

        emit_addiu(SP, SP, -12, str);
        emit_store(FP, 3, SP, str);
        emit_store(SELF, 2, SP, str);
        emit_store(RA, 1, SP, str);
        emit_addiu(FP, SP, 4, str);
        emit_move(SELF, ACC, str);

        if (cls->get_name() != Object) {
            // initialize parent class first
            str << "\tjal " << cls->get_parent() << CLASSINIT_SUFFIX << endl;
        }

        Environment env;
        env.set_cls(cls);
        for (auto attr : cls->all_attrs) {
            env.add_cls_attr(attr);
        }

        Features features = cls->get_features();
        for (int i = features->first(); features->more(i); i = features->next(i)) {
            attr_class *at = dynamic_cast<attr_class *>(features->nth(i));

            if (at && !at->get_init()->is_empty()) {
                at->get_init()->code(str, env);
                emit_store(ACC, DEFAULT_OBJFIELDS + env.get_cls_attr_pos(at->get_name()), SELF, str);
            }
        }

        emit_move(ACC, SELF, str);
        emit_load(FP, 3, SP, str);
        emit_load(SELF, 2, SP, str);
        emit_load(RA, 1, SP, str);
        emit_addiu(SP, SP, 12, str);

        emit_return(str);
    }
}

void CgenClassTable::code_methods()
{
    for(std::vector<Class_>::size_type i = 0; i < cls_ordered.size(); i++) {
        Class_ cls = cls_ordered[i];
        Symbol name = cls->get_name();

        if (!is_basic_class(name)) {
            Environment env;
            env.set_cls(cls);
            for (auto attr : cls->all_attrs) {
                env.add_cls_attr(attr);
            }

            Features features = cls->get_features();

            for (int i = features->first(); features->more(i); i = features->next(i)) {
                method_class *method = dynamic_cast<method_class *>(features->nth(i));
                if (method) {
                    method->code(str, env);
                }
            }
        }
    }
}

void CgenClassTable::code()
{
    if (cgen_debug) cout << "coding global data" << endl;
    code_global_data();

    if (cgen_debug) cout << "choosing gc" << endl;
    code_select_gc();

    if (cgen_debug) cout << "coding constants" << endl;
    code_constants();

    //                 Add your code to emit
    //                   - prototype objects
    //                   - class_nameTab
    //                   - dispatch tables
    //

    code_class_name_tab();
    code_class_parent_tab();
    code_class_obj_tab();
    code_dispatch_tables();
    code_prototypes();

    //if (cgen_debug) cout << "coding global text" << endl;
    code_global_text();

    //                 Add your code to emit
    //                   - object initializer
    //                   - the class methods
    //                   - etc...

    code_initializers();
    code_methods();
}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus)
{
    stringtable.add_string(name->get_string());          // Add class name to string table
}


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************
//
void method_class::code(ostream &s, Environment &env)
{
    emit_method_ref(env.get_cls()->get_name(), name, s);
    s << LABEL;

    // make space in the stack
    emit_addiu(SP, SP, -(DEFAULT_OBJFIELDS * 4), s);
    // save old $fp, self and $ra values
    emit_store(FP, 3, SP, s);
    emit_store(SELF, 2, SP, s);
    emit_store(RA, 1, SP, s);
    // set fp to point to old $ra
    emit_addiu(FP, SP, 4, s);
    // move acc to self
    emit_move(SELF, ACC, s);

    for(int i = formals->first(); formals->more(i); i = formals->next(i)) {
        env.add_mth_arg(formals->nth(i));
    }

    expr->code(s, env);

    // restore $fp, self and $ra
    emit_load(FP, 3, SP, s);
    emit_load(SELF, 2, SP, s);
    emit_load(RA, 1, SP, s);

    // destroy the stack frame
    emit_addiu(SP, SP, (DEFAULT_OBJFIELDS + env.get_mth_args_size()) * 4, s);
    env.clear_mth_args();

    s << RET << "\n";
}

void assign_class::code(ostream &s, Environment &env) {
    expr->code(s, env);
    int pos, offset;

    pos = env.get_let_var_pos_rev(name);
    if (pos != -1) {
        offset = pos + 1;
        emit_store(ACC, offset, SP, s);

        if (cgen_Memmgr == GC_GENGC) {
            emit_addiu(A1, SP, 4 * offset, s);
            emit_gc_assign(s);
        }
        return;
    }

    pos = env.get_arg_pos(name);
    if (pos != -1) {
        offset = 2 + env.get_mth_args_size() - pos;
        emit_store(ACC, offset, FP, s);

        if (cgen_Memmgr == GC_GENGC) {
            emit_addiu(A1, FP, 4 * offset, s);
            emit_gc_assign(s);
        }
        return;
    }

    pos = env.get_cls_attr_pos(name);
    if (pos != -1) {
        offset = DEFAULT_OBJFIELDS + pos;
        emit_store(ACC, offset, SELF, s);

        if (cgen_Memmgr == GC_GENGC) {
            emit_addiu(A1, SELF, 4 * offset, s);
            emit_gc_assign(s);
        }
        return;
    }
}

void static_dispatch_class::code(ostream &s, Environment &env) {
    int num_params = 0;

    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        actual->nth(i)->code(s, env);
        emit_push(ACC, s);
        env.push_stack_symbol(No_type);

        num_params++;
    }

    // $a0 = expr_obj
    expr->code(s, env);

    // catch dispatch on void
    // if $a0 != 0 then jump to labelx
    emit_bne(ACC, ZERO, label_num, s);

    //s << "\tla\t" << ACC << " ";
    emit_partial_load_address(ACC, s);
    stringtable.lookup_string(env.get_cls()->get_filename()->get_string())->code_ref(s);
    s << endl;
    emit_load_imm(T1, get_line_number(), s);
    emit_jal("_dispatch_abort", s);

    // labelx...
    emit_label_def(label_num++, s);

    // $t1 = type_name_dispatch_pointer
    emit_load_address(T1, (char *) (std::string(type_name->get_string()) + DISPTAB_SUFFIX).c_str(), s);

    Class_ cls = class_map[type_name];

    int i;
    for (i = 0; i < (int) cls->all_methods.size(); i++) {
        if (cls->all_methods[i].second->get_name() == name) {
            break;
        }
    }

    // $t1 += offset_to_proper_func
    emit_load(T1, i, T1, s);
    // set $ra to next instruction and jump to $t1
    emit_jalr(T1, s);

    for (int i = 0; i < num_params; i++) {
        // this simply removes the symbols from the vector
        // the callee is responsible for actually increasing the $sp
        env.pop_stack_symbol();
    }
}

void dispatch_class::code(ostream &s, Environment &env) {
    int num_params = 0;

    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        actual->nth(i)->code(s, env);
        emit_push(ACC, s);
        env.push_stack_symbol(No_type);

        num_params++;
    }

    // $a0 = expr_obj
    expr->code(s, env);

    // catch dispatch on void
    // if $a0 != 0 then jump to labelx
    emit_bne(ACC, ZERO, label_num, s);

    emit_partial_load_address(ACC, s);
    stringtable.lookup_string(env.get_cls()->get_filename()->get_string())->code_ref(s);
    s << endl;
    emit_load_imm(T1, get_line_number(), s);
    emit_jal("_dispatch_abort", s);

    // labelx...
    emit_label_def(label_num++, s);

    // $t1 = expr_obj.dispatch_pointer
    emit_load(T1, 2, ACC, s);

    Class_ cls = env.get_cls();
    if (expr->get_type() != SELF_TYPE) {
        cls = class_map[expr->get_type()];
    }

    int i;
    for (i = 0; i < (int) cls->all_methods.size(); i++) {
        if (cls->all_methods[i].second->get_name() == name) {
            break;
        }
    }

    // $t1 += offset_to_proper_func
    emit_load(T1, i, T1, s);
    // set $ra to next instruction and jump to $t1
    emit_jalr(T1, s);

    for (int i = 0; i < num_params; i++) {
        // this simply removes the symbols from the vector
        // the callee is responsible for actually increasing the $sp
        env.pop_stack_symbol();
    }
}

void cond_class::code(ostream &s, Environment &env) {
    pred->code(s, env);
    emit_fetch_int(T1, ACC, s);

    int label_false = label_num++;
    int label_end = label_num++;

    emit_beq(T1, ZERO, label_false, s);
    then_exp->code(s, env);
    emit_branch(label_end, s);

    emit_label_def(label_false, s);
    else_exp->code(s, env);

    emit_label_def(label_end, s);
}

void loop_class::code(ostream &s, Environment &env) {
    int label_loop = label_num++;
    int label_exit = label_num++;

    emit_label_def(label_loop, s);

    pred->code(s, env);

    emit_fetch_int(T1, ACC, s);
    emit_beq(T1, ZERO, label_exit, s);

    body->code(s, env);
    emit_branch(label_loop, s);

    emit_label_def(label_exit, s);

    // loop always returns void
    emit_move(ACC, ZERO, s);
}

void typcase_class::code(ostream &s, Environment &env) {
    expr->code(s, env);

    // push expr onto the stack
    // the name of the env whichs binds to this value will be pushed later
    emit_push(ACC, s);

    // check case on void
    emit_bne(ACC, ZERO, label_num, s);

    emit_partial_load_address(ACC, s);
    stringtable.lookup_string(env.get_cls()->get_filename()->get_string())->code_ref(s);
    s << endl;
    emit_load_imm(T1, get_line_number(), s);
    emit_jal("_case_abort2", s);

    // expr was not void, execution continues here. $a0 holds expr object
    emit_label_def(label_num++, s);

    int label_begin = label_num++;
    int label_end = label_num++;
    int label_tag_is_valid = label_num++;

    // $t1 = expr_obj.tag
    emit_load(T1, TAG_OFFSET, ACC, s);

    // everytime we jump back here, T1 contains the tag of a new class
    // if T1 is ever equal to INVALID_CLASSTAG it means that the case statement
    // has no match which is a runtime error
    emit_label_def(label_begin, s);
    emit_load_imm(T2, INVALID_CLASSTAG, s);
    emit_bne(T1, T2, label_tag_is_valid, s);
    emit_jal("_case_abort", s);

    // let's check if any of the branches have a type that matches exactly
    // the class tag of T1
    emit_label_def(label_tag_is_valid, s);

    int label_branch_0 = label_num;

    for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
        // $t2 = branch_i.tag
        emit_load_imm(T2, get_class_tag(cases->nth(i)->get_type_decl()), s);
        // if $t1 == $t2 jump to the label for the corresponding branch
        emit_beq(T1, T2, label_num++, s);
    }

    // none of the tags matched, so let's try get t1's parent and try again
    // we'll go all the way to the top of the class hierarhy until we eventually
    // have a match or have a runtime error
    emit_load_address(T2, CLASSPARENTTAB, s);

    // calculate the offset in the parent_table. $t1 holds the type's tag
    // $t3 = $t1 * 4
    emit_load_imm(T3, 4, s);
    emit_mul(T3, T1, T3, s);
    // $t2 += $t3
    emit_add(T2, T2, T3, s);

    // set $t1 to the class tag of the parent of the old $t1 value
    emit_load(T1, 0, T2, s);

    // let's go back and try again
    emit_branch(label_begin, s);

    // finally generate code for each branch
    for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
        emit_label_def(label_branch_0++, s);

        // bind the branch var name to expr object that is already in the stack
        env.push_stack_symbol(cases->nth(i)->get_name());

        cases->nth(i)->get_expr()->code(s, env);

        env.pop_stack_symbol();
        emit_branch(label_end, s);
    }

    emit_label_def(label_end, s);

    // pop expr from the stack
    emit_addiu(SP, SP, 4, s);
}

void block_class::code(ostream &s, Environment &env) {
    for (int i = body->first(); body->more(i); i = body->next(i)) {
        body->nth(i)->code(s, env);
    }
}

void let_class::code(ostream &s, Environment &env) {
    init->code(s, env);

    if (init->is_empty()) {
        if (type_decl == Str) {
            emit_load_string(ACC, stringtable.lookup_string(""), s);
        } else if (type_decl == Int) {
            emit_load_int(ACC, inttable.lookup_string("0"), s);
        } else if (type_decl == Bool) {
            emit_load_bool(ACC, BoolConst(0), s);
        }
    }

    emit_push(ACC, s);
    env.push_stack_symbol(identifier);

    body->code(s, env);

    emit_addiu(SP, SP, 4, s);
    env.pop_stack_symbol();
}

void plus_class::code(ostream &s, Environment &env) {
    // eval e1 and put the result on the stack
    e1->code(s, env);
    emit_push(ACC, s);
    env.push_stack_symbol(No_type);

    // eval e2 and copy the object; the new object is in $a0
    e2->code(s, env);
    emit_jal("Object.copy", s);

    // $t1 = stack_pop(); $t1 points to e1 object
    emit_addiu(SP, SP, 4, s);
    emit_load(T1, 0, SP, s);
    env.pop_stack_symbol();

    // $t2 = $a0; $t2 points to e2 object
    emit_move(T2, ACC, s);

    // $t1 = $t1.int
    emit_fetch_int(T1, T1, s);
    // $t2 = $t2.int
    emit_fetch_int(T2, T2, s);

    // $t3 = $t1 + t2
    emit_add(T3, T1, T2, s);
    // $a0.int = $t3
    emit_store(T3, 3, ACC, s);
}

void sub_class::code(ostream &s, Environment &env) {
    e1->code(s, env);
    emit_push(ACC, s);
    env.push_stack_symbol(No_type);

    e2->code(s, env);
    emit_jal("Object.copy", s);

    emit_addiu(SP, SP, 4, s);
    emit_load(T1, 0, SP, s);
    env.pop_stack_symbol();

    emit_move(T2, ACC, s);

    emit_fetch_int(T1, T1, s);
    emit_fetch_int(T2, T2, s);

    emit_sub(T3, T1, T2, s);
    emit_store(T3, 3, ACC, s);
}

void mul_class::code(ostream &s, Environment &env) {
    e1->code(s, env);
    emit_push(ACC, s);
    env.push_stack_symbol(No_type);

    e2->code(s, env);
    emit_jal("Object.copy", s);

    emit_addiu(SP, SP, 4, s);
    emit_load(T1, 0, SP, s);
    env.pop_stack_symbol();

    emit_move(T2, ACC, s);

    emit_fetch_int(T1, T1, s);
    emit_fetch_int(T2, T2, s);

    emit_mul(T3, T1, T2, s);
    emit_store(T3, 3, ACC, s);
}

void divide_class::code(ostream &s, Environment &env) {
    e1->code(s, env);
    emit_push(ACC, s);
    env.push_stack_symbol(No_type);

    e2->code(s, env);
    emit_jal("Object.copy", s);

    emit_addiu(SP, SP, 4, s);
    emit_load(T1, 0, SP, s);
    env.pop_stack_symbol();

    emit_move(T2, ACC, s);

    emit_fetch_int(T1, T1, s);
    emit_fetch_int(T2, T2, s);

    emit_div(T3, T1, T2, s);
    emit_store(T3, 3, ACC, s);
}

void neg_class::code(ostream &s, Environment &env) {
    e1->code(s, env);
    emit_jal("Object.copy", s);

    emit_fetch_int(T1, ACC, s);
    emit_neg(T1, T1, s);
    emit_store(T1, 3, ACC, s);
}

void lt_class::code(ostream &s, Environment &env) {
    e1->code(s, env);
    emit_push(ACC, s);
    env.push_stack_symbol(No_type);

    e2->code(s, env);

    emit_addiu(SP, SP, 4, s);
    emit_load(T1, 0, SP, s);
    env.pop_stack_symbol();

    emit_move(T2, ACC, s);

    emit_fetch_int(T1, T1, s);
    emit_fetch_int(T2, T2, s);

    emit_load_bool(ACC, BoolConst(1), s);
    emit_blt(T1, T2, label_num, s);

    emit_load_bool(ACC, BoolConst(0), s);
    emit_label_def(label_num++, s);
}

void eq_class::code(ostream &s, Environment &env) {
    e1->code(s, env);
    emit_push(ACC, s);
    env.push_stack_symbol(No_type);

    e2->code(s, env);

    emit_addiu(SP, SP, 4, s);
    emit_load(T1, 0, SP, s);
    env.pop_stack_symbol();

    emit_move(T2, ACC, s);

    if (e1->type == Int || e1->type == Str || e1->type == Bool) {
        emit_load_bool(ACC, BoolConst(1), s);
        emit_load_bool(A1, BoolConst(0), s);
        emit_jal("equality_test", s);
        return;
    }

    emit_load_bool(ACC, BoolConst(1), s);
    emit_beq(T1, T2, label_num, s);
    emit_load_bool(ACC, BoolConst(0), s);
    emit_label_def(label_num++, s);
}

void leq_class::code(ostream &s, Environment &env) {
    e1->code(s, env);
    emit_push(ACC, s);
    env.push_stack_symbol(No_type);

    e2->code(s, env);

    emit_addiu(SP, SP, 4, s);
    emit_load(T1, 0, SP, s);
    env.pop_stack_symbol();

    emit_move(T2, ACC, s);

    emit_fetch_int(T1, T1, s);
    emit_fetch_int(T2, T2, s);

    emit_load_bool(ACC, BoolConst(1), s);
    emit_bleq(T1, T2, label_num, s);

    emit_load_bool(ACC, BoolConst(0), s);
    emit_label_def(label_num++, s);
}

void comp_class::code(ostream &s, Environment &env) {
    e1->code(s, env);
    emit_fetch_int(T1, ACC, s);

    emit_load_bool(ACC, BoolConst(1), s);

    emit_beq(T1, ZERO, label_num, s);
    emit_load_bool(ACC, BoolConst(0), s);

    emit_label_def(label_num++, s);
}

void int_const_class::code(ostream& s, Environment &env) {
    emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream& s, Environment &env) {
    emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream& s, Environment &env) {
    emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s, Environment &env) {
    if (type_name != SELF_TYPE) {
        emit_load_address(ACC, (char *) (std::string(type_name->get_string()) + PROTOBJ_SUFFIX).c_str(), s);
        emit_jal("Object.copy", s);
        emit_jal((char *) (std::string(type_name->get_string()) + CLASSINIT_SUFFIX).c_str(), s);
        return;
    }

    emit_load_address(T1, CLASSOBJTAB, s);

    // $t2 = self.tag
    emit_load(T2, 0, SELF, s);
    // $t2 = $t2 * 8
    emit_load_imm(T3, 8, s);
    emit_mul(T2, T2, T3, s);
    // $t1 += offset in the CLASSOBJTAB
    emit_addu(T1, T1, T2, s);
    // $t1 now points to SELF_TYPE_CLASS_protObj

    // push $t1 to the stack
    emit_push(T1, s);

    emit_load(ACC, 0, T1, s);
    emit_jal("Object.copy", s);

    // pop old pointer from the stack to $t1
    emit_addiu(SP, SP, 4, s);
    emit_load(T1, 0, SP, s);

    // $t1 += 1 so it now points to SELF_TYPE_CLASS_init
    emit_load(T1, 1, T1, s);
    emit_jalr(T1, s);
}

void isvoid_class::code(ostream &s, Environment &env) {
    e1->code(s, env);
    emit_move(T1, ACC, s);

    emit_load_bool(ACC, BoolConst(1), s);

    emit_beq(T1, ZERO, label_num, s);
    emit_load_bool(ACC, BoolConst(0), s);

    emit_label_def(label_num++, s);
}

void no_expr_class::code(ostream &s, Environment &env) {
    emit_move(ACC, ZERO, s);
}

void object_class::code(ostream &s, Environment &env) {
    int pos;

    pos = env.get_let_var_pos_rev(name);
    if (pos != -1) {
        emit_load(ACC, pos + 1, SP, s);
        return;
    }

    pos = env.get_arg_pos(name);
    if (pos != -1) {
        emit_load(ACC, 2 + env.get_mth_args_size() - pos, FP, s);
        return;
    }

    pos = env.get_cls_attr_pos(name);
    if (pos != -1) {
        emit_load(ACC, DEFAULT_OBJFIELDS + pos, SELF, s);
        return;
    }

    // name == self
    emit_move(ACC, SELF, s);
}
