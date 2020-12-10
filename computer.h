#ifndef COMPUTER_LIBRARY2_H
#define COMPUTER_LIBRARY2_H

#include <cstdlib>
#include <type_traits>
#include <array>

//===============================   HELPERS   ===============================//
using std::pair;
using std::array;
using ull = uint64_t;
using ll = int64_t;

using IdType = ull;
using PositionType = ull;
using AddressType = ull;

template <ull ID> struct CheckId {
    static constexpr bool run() {
        char c = static_cast<char>(ID);
        if (c != '\0') {
            return false;
        }
        ull div = 8;
        for (; div < 64; div += 8) {
            c = static_cast<char>(ID >> div);
            if (!('a' <= c && c <= 'z') && !('0' <= c && c <= '9')) {
                break;
            }
        }
        if (div == 8) {
            return false;
        }
        for (; div < 64; div += 8) {
            c = static_cast<char>(ID >> div);
            if (c != '\0') {
                return false;
            }
        }
        return true;
    }
};

enum Types {
    UndefType,
    NumType,
    LabType,
    DefType
};

template <class F, class S> struct Pair {
    const F first;
    const S second;
    constexpr Pair(F f, S s) : first(f), second(s) {}
};

template <class MemType, ull MemSize, ull LabelCnt, ull DefCnt, ull codeLength>
struct State {
    PositionType pos = 0;
    array<Pair<IdType, PositionType>, LabelCnt> labels;
    array<Pair<IdType, AddressType>, DefCnt> variables;
    array<MemType, MemSize> memory = {0};
    bool SF = false, ZF = false;

    constexpr State() = default;
    constexpr void init(PositionType newPos, bool sf, bool zf) {
        pos = newPos, SF = sf, ZF = zf;
    }

    constexpr PositionType getLabPos(const IdType id) {
        PositionType minPosition = codeLength;
        for (auto labelInfo : labels) {
            if (labelInfo.first == id) {
                minPosition = min(minPosition, labelInfo.second);
            }
        }
        static_assert(minPosition < codeLength);
        return minPosition;
    }
    constexpr AddressType getMemAddr(const IdType id) {
        AddressType minAddress = MemSize;
        for (auto variableInfo : variables) {
            if (variableInfo.first == id) {
                minAddress = min(minAddress, variableInfo.second);
            }
        }
        static_assert(minAddress < MemSize, "Variable does not exist.");
        return minAddress;
    }
    constexpr void setMem(const AddressType addr, const MemType val) {
        static_assert(addr < MemSize, "Address out of memory.");
        memory[addr] = val;
    }
    constexpr MemType getMem(const AddressType addr) {
        static_assert(addr < MemSize, "Address out of memory.");
        return memory[addr];
    }
    constexpr void setSF(bool sf) {
        SF = sf;
    }
    constexpr void setZF(bool zf) {
        ZF = zf;
    }
    constexpr void setPos(const PositionType newPos) {
        pos = newPos;
    }
    constexpr PositionType getPos() {
        return pos;
    }

};


struct Statement {
    template<class State>
    static constexpr void apply(State& state) {};
    static constexpr Types type = UndefType;
};
struct RExpresion {
    template<class State>
    static constexpr ll rval(const State& state) {
        return 0;
    }
};
struct LExpresion {
    template<class State>
    static constexpr ll lval(const State& state) {
        return 0;
    }
};
struct Expresion : RExpresion, LExpresion {};

//==============================   SEMANTICS   ==============================//

constexpr ull Id(const char* name) {
    size_t len = 0;
    ull hashName = 0;
    for (; len < 6 && name[len] != '\0'; len++) {
        char c = name[len];
        if ('A' <= c && c <= 'Z') {
            c += 'a' - 'A';
        }
        hashName <<= 8;
        hashName += c;
    }
    hashName <<= 8;
    hashName += name[len];
    return hashName;
}

template <ull ID>
struct Label : Statement {
    static_assert(CheckId<ID>::run(), "Wrong name of identifier.");
    template<class State>
    static constexpr void apply(State& state) {}
    static constexpr ull id = ID;
    static constexpr Types type = LabType;
};

template <ull ID>
struct Jmp : Statement {
    static_assert(CheckId<ID>::run(), "Wrong name of identifier.");
    template<class State>
    static constexpr void apply(State& state) {
        state.setPos(state.getLabPos(ID));
    }
    static constexpr Types type = UndefType;
};

template <ull ID>
struct Js : Jmp<ID> {
    template<class State>
    static constexpr void apply(State& state) {
        if (state.getSF()) {
            Jmp<ID>::apply(state);
        }
    }
};

template <ull ID>
struct Jz : Jmp<ID> {
    template<class State>
    static constexpr void apply(State& state) {
        if (state.getZF()) {
            Jmp<ID>::apply(state);
        }
    }
};

template <ull ID, typename Num>
struct D : Statement {
    static_assert(CheckId<ID>::run(), "Wrong name of identifier.");
    static_assert(Num::type == NumType, "Definition value can only be a numeric");
    template<class State>
    static constexpr void apply(State& state) {}
    static constexpr ull id = ID;
    template<class State>
    static constexpr ll numericVal(State& state) {
        return Num::rval(state);
    }
    static constexpr Types type = DefType;
};

template <auto VAL>
struct Num : RExpresion {
    template<class State>
    static constexpr ll rval(const State& state) {
        return VAL;
    }
    static constexpr Types type = NumType;
};

template <typename ADDR>
struct Mem : Expresion {
    template<class State>
    static constexpr ll lval(const State& state); //TODO
    template<class State>
    static constexpr ll rval(const State& state); //TODO
};

template <ull ID>
struct Lea : RExpresion {
    static_assert(CheckId<ID>::run(), "Wrong name of identifier.");
    template<class State>
    static constexpr ll rval(const State& state); //TODO
};

// TODO - the rest of classes


template<class... Inst> struct Program;

//===============================   RUNNERS   ===============================//

template <class Prog, class Filter> struct Counter;
template <class Stmt, class... Stmts, class Filter> struct Counter<Program<Stmt, Stmts...>, Filter> {
    static constexpr ull run() {
        ull curr = Filter::template check<Stmt>() ? 1 : 0;
        ull tail = Counter<Program<Stmts...>, Filter>::run();
        return curr + tail;
    }
};
template <class Filter> struct Counter<Program<>, Filter> {
    static constexpr ull run() {
        return 0;
    }
};

template <Types T> struct TypesFilter {
    template <class C>
    static constexpr bool check() {
        return C::type == T;
    }
};

// Gets Pos-th statement from program
template <ull Pos, class C> struct FoldExprJump;
template <ull Pos, class Stmt, class... Stmts> struct FoldExprJump<Pos, Program<Stmt, Stmts...>> {
    typedef typename FoldExprJump<Pos - 1, Program<Stmts...>>::statement statement;
};
template <class Stmt, class... Stmts> struct FoldExprJump<0, Program<Stmt, Stmts...>> {
    typedef Stmt statement;
};

template <Types T, class Prog> struct Collect;
template <Types T, class Stmt, class... Stmts> struct Collect<T, Program<Stmt, Stmts...>> {
    template<class State>
    static constexpr void run(State& state, const ull index = 0) {
        if (T == LabType) {
            if (Stmt::type == LabType) {
                state.labels[index] = Pair<IdType, PositionType>(Stmt::id, state.pos);
            }
            state.pos++;
            return Collect<LabType, Program<Stmts...>>::run(state, index + 1);
        }
        if (T == DefType) {
            if (Stmt::type == DefType) {
                state.variables[index] = Pair<IdType, AddressType>(Stmt::id, index);
                state.memory[index] = Stmt::numericVal(state);
            }
            return Collect<DefType, Program<Stmts...>>::run(state, index + 1);
        }
    }
};
template <Types T> struct Collect<T, Program<>> {
    template<class State>
    static constexpr void run(State& state, const ull index = 0) {}
};

template <class Prog> struct CodeLength;
template <class Stmt, class... Stmts> struct CodeLength<Program<Stmt, Stmts...>> {
    static constexpr ull get() {
        return CodeLength<Program<Stmts...>>::get() + 1;
    }
};
template <> struct CodeLength<Program<>> {
    static constexpr ull get() {
        return 0;
    }
};

template <class Prog, ull codeLength> struct Runner {
    template<class State>
    static constexpr void run(State& state) {
        while (true) {
            const ull currPos(state.getPos());
            if (currPos == codeLength) {
                return;
            }
            FoldExprJump<currPos, Prog>::statement::apply(state); //TODO currPos is not const
        }
    }
};


template<ull MemSize, class MemType>
class Computer {
public:
    template<class Prog>
    static constexpr std::array<MemType, MemSize> boot() {
        static_assert(std::is_integral<MemType>::value, "Integral type required.");
        const ull labelCnt(Counter<Prog, TypesFilter<LabType>>::run());
        const ull defCnt(Counter<Prog, TypesFilter<DefType>>::run());
        const ull codeLength(CodeLength<Prog>::get());
        static_assert(defCnt <= MemSize, "Out of memory.");

        typedef State<MemType, MemSize, labelCnt, defCnt, codeLength> StateType;
        StateType state; //TODO problem with constructing state
        state.init(0, false, false);
        Collect<LabType, Prog>::template run<StateType>(state);
        Collect<DefType, Prog>::template run<StateType>(state);
        state.init(0, false, false);
        Runner<Prog, codeLength>::template run<StateType>(state);

        return state.memory;
    }
};



#endif //COMPUTER_LIBRARY2_H
