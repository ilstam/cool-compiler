#include <assert.h>
#include <stdio.h>
#include <vector>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
    List<CgenNode> *nds;
    ostream& str;
    int stringclasstag;
    int intclasstag;
    int boolclasstag;


    // The following methods emit code for
    // constants and global declarations.

    void code_global_data();
    void code_global_text();
    void code_bools(int);
    void code_select_gc();
    void code_constants();

    void code_class_name_tab();
    void code_dispatch_tables();
    void code_prototypes();
    void code_initializers();
    void code_methods();

    // The following creates an inheritance graph from
    // a list of classes.  The graph is implemented as
    // a tree of `CgenNode', and class names are placed
    // in the base class symbol table.

    void install_basic_classes();
    void install_class(CgenNodeP nd);
    void install_classes(Classes cs);
    void build_inheritance_tree();
    void set_relations(CgenNodeP nd);
public:
    CgenClassTable(Classes, ostream& str);
    void code();
    CgenNodeP root();
};


class CgenNode : public class__class {
private:
    CgenNodeP parentnd;                        // Parent of class
    List<CgenNode> *children;                  // Children of class
    Basicness basic_status;                    // `Basic' if class is basic
                                               // `NotBasic' otherwise

public:
    CgenNode(Class_ c,
             Basicness bstatus,
             CgenClassTableP class_table);

    void add_child(CgenNodeP child);
    List<CgenNode> *get_children() { return children; }
    void set_parentnd(CgenNodeP p);
    CgenNodeP get_parentnd() { return parentnd; }
    int basic() { return (basic_status == Basic); }
};

class BoolConst {
private:
    int val;
public:
    BoolConst(int);
    void code_def(ostream&, int boolclasstag);
    void code_ref(ostream&) const;
};

struct Environment {
    Class_ cls;
    std::vector<Symbol> let_vars;

    int add_let_var(Symbol name) {
        let_vars.push_back(name);
        return (int) let_vars.size() - 1;
    }

    int pop_let_var() {
        let_vars.pop_back();
        return (int) let_vars.size() - 1;
    }
};
