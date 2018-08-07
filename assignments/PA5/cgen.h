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
    void code_class_parent_tab();
    void code_class_obj_tab();
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

class Environment {
    Class_ cls;
    std::vector<attr_class *> cls_attrs;
    std::vector<Formal> mth_args;
    std::vector<Symbol> stack_symbols;

public:
    Class_ get_cls() {
        return cls;
    }

    void set_cls(Class_ cls) {
        this->cls = cls;
    }

    int get_cls_attrs_size() {
        return cls_attrs.size();
    }

    int get_mth_args_size() {
        return mth_args.size();
    }

    void add_cls_attr(attr_class *attr) {
        cls_attrs.push_back(attr);
    }

    void add_mth_arg(Formal formal) {
        mth_args.push_back(formal);
    }

    void clear_mth_args() {
        mth_args.clear();
    }

    void push_stack_symbol(Symbol name) {
        stack_symbols.push_back(name);
    }

    void pop_stack_symbol() {
        stack_symbols.pop_back();
    }

    // returns symbol's position from the END of the vector or -1 if not found
    // e.g. if "name" corresponds to the last Symbol of the stack_symbols vector
    //      the function will return 0
    int get_let_var_pos_rev(Symbol name) {
        // the vector is searched in reverse order because 2 symbols with the
        // same name might have been pushed in the stack
        for (int i = stack_symbols.size() - 1; i >= 0; i--) {
            if (stack_symbols[i] == name) {
                return stack_symbols.size() - 1 - i;
            }
        }
        return -1;
    }

    // returns argument's position on the vector (starting from 0) or -1 if not found
    int get_arg_pos(Symbol name) {
        for (int i = 0; i < (int) mth_args.size(); i++) {
            if (mth_args[i]->get_name() == name) {
                return i;
            }
        }
        return -1;
    }

    // returns attribute's position on the vector (starting from 0) or -1 if not found
    int get_cls_attr_pos(Symbol name) {
        for (int i = 0; i < (int) cls_attrs.size(); i++) {
            if (cls_attrs[i]->get_name() == name) {
                return i;
            }
        }
        return -1;
    }
};
