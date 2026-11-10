#include <algorithm>
#include <cctype>
#include <iomanip>
#include <iostream>
#include <map>
#include <memory>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

// -----------------------------------------
// Лексический анализ
// -----------------------------------------

enum class TokenKind { Var, Not, And, Or, Imp, LParen, RParen, End };

class Token {
    TokenKind kind;
    std::string text;
    size_t pos;
public:
    Token(const TokenKind& kind, std::string text, size_t pos) : kind(kind), text(std::move(text)), pos(pos) {}
    std::string ToString() const { return text; }
    size_t GetPos() const { return pos; }
    TokenKind GetKind() const { return kind; }
};

struct ParseError : std::runtime_error {
    size_t pos;
    ParseError(const std::string& msg, size_t p) : std::runtime_error(msg), pos(p) {}
};

std::vector<Token> Tokenize(const std::string& s) {
    static const std::vector<std::pair<std::string, TokenKind>> ops = {
        {"->", TokenKind::Imp},  {"→", TokenKind::Imp},
        {"&",  TokenKind::And},  {"∧", TokenKind::And},
        {"|",  TokenKind::Or},   {"∨", TokenKind::Or},
        {"~",  TokenKind::Not},  {"¬", TokenKind::Not},
        {"(",  TokenKind::LParen},
        {")",  TokenKind::RParen},
    };

    std::vector<Token> out;
    size_t i = 0;
    while (i < s.size()) {
        unsigned char c = s[i];
        if (std::isspace(c)) { ++i; continue; }

        if (std::isalpha(c) || c == '_') {
            size_t start = i;
            while (i < s.size() && (std::isalnum((unsigned char)s[i]) || s[i] == '_')) ++i;
            out.push_back({TokenKind::Var, s.substr(start, i - start), start});
            continue;
        }

        bool matched = false;
        for (const auto& [text, kind] : ops) {
            if (s.compare(i, text.size(), text) == 0) {
                out.push_back({kind, text, i});
                i += text.size();
                matched = true;
                break;
            }
        }
        if (!matched)
            throw ParseError("неизвестный символ '" + std::string(1, s[i]) + "'", i);
    }
    out.push_back({TokenKind::End, "", s.size()});
    return out;
}

std::string describe(const Token& t) {
    return t.GetKind() == TokenKind::End ? "неожиданный конец формулы"
                                         : "неожиданный символ '" + t.ToString() + "'";
}

// -----------------------------------------
// Дерево разбора (AST)
// -----------------------------------------

enum class NodeKind { Var, Not, And, Or, Imp };

class Node {
    NodeKind kind;
    std::string name;
    std::unique_ptr<Node> left;
    std::unique_ptr<Node> right;
    Node() {}

public:
    NodeKind GetKind() const { return kind; }
    const std::string& GetName() const { return name; }
    const Node* GetLeft() const { return left.get(); }
    const Node* GetRight() const { return right.get(); }

    std::string ToString() const {
        switch (kind) {
            case NodeKind::Var: return name;
            case NodeKind::Not: return "~" + left->ToString();
            case NodeKind::And: return "(" + left->ToString() + " & " + right->ToString() + ")";
            case NodeKind::Or:  return "(" + left->ToString() + " | " + right->ToString() + ")";
            case NodeKind::Imp: return "(" + left->ToString() + " -> " + right->ToString() + ")";
        }
        return name;
    }

    static std::unique_ptr<Node> Var(std::string n) {
        auto p = std::unique_ptr<Node>(new Node());
        p->kind = NodeKind::Var;
        p->name = std::move(n);
        return p;
    }
    static std::unique_ptr<Node> Unary(NodeKind k, std::unique_ptr<Node> a) {
        auto p = std::unique_ptr<Node>(new Node());
        p->kind = k;
        p->left = std::move(a);
        return p;
    }
    static std::unique_ptr<Node> Binary(NodeKind k, std::unique_ptr<Node> a, std::unique_ptr<Node> b) {
        auto p = std::unique_ptr<Node>(new Node());
        p->kind = k;
        p->left = std::move(a);
        p->right = std::move(b);
        return p;
    }
};

enum class Symbol {
    Var, Not, And, Or, Imp, LParen, RParen, End,
    I, I1, D, D1, C, C1, N, A,
    MkImp, MkOr, MkAnd, MkNot
};

bool isTerminal(Symbol s) { return s <= Symbol::End; }
bool isAction(Symbol s) { return s >= Symbol::MkImp; }

NodeKind kindOf(Symbol action) {
    switch (action) {
        case Symbol::MkImp: return NodeKind::Imp;
        case Symbol::MkOr:  return NodeKind::Or;
        default:            return NodeKind::And;
    }
}

std::map<std::pair<Symbol, TokenKind>, std::vector<Symbol>> BuildTable() {
    std::map<std::pair<Symbol, TokenKind>, std::vector<Symbol>> M;
    for (TokenKind t : {TokenKind::Not, TokenKind::Var, TokenKind::LParen}) {
        M[{Symbol::I, t}] = {Symbol::D, Symbol::I1};
        M[{Symbol::D, t}] = {Symbol::C, Symbol::D1};
        M[{Symbol::C, t}] = {Symbol::N, Symbol::C1};
    }
    M[{Symbol::I1, TokenKind::Imp}] = {Symbol::Imp, Symbol::D, Symbol::MkImp, Symbol::I1};
    for (TokenKind t : {TokenKind::Or, TokenKind::RParen, TokenKind::End}) M[{Symbol::I1, t}] = {};

    M[{Symbol::D1, TokenKind::Or}] = {Symbol::Or, Symbol::C, Symbol::MkOr, Symbol::D1};
    for (TokenKind t : {TokenKind::Imp, TokenKind::RParen, TokenKind::End}) M[{Symbol::D1, t}] = {};

    M[{Symbol::C1, TokenKind::And}] = {Symbol::And, Symbol::N, Symbol::MkAnd, Symbol::C1};
    for (TokenKind t : {TokenKind::Or, TokenKind::Imp, TokenKind::RParen, TokenKind::End}) M[{Symbol::C1, t}] = {};

    M[{Symbol::N, TokenKind::Not}] = {Symbol::Not, Symbol::N, Symbol::MkNot};
    for (TokenKind t : {TokenKind::Var, TokenKind::LParen}) M[{Symbol::N, t}] = {Symbol::A};

    M[{Symbol::A, TokenKind::Var}] = {Symbol::Var};
    M[{Symbol::A, TokenKind::LParen}] = {Symbol::LParen, Symbol::I, Symbol::RParen};
    return M;
}

class Parser {
    std::map<std::pair<Symbol, TokenKind>, std::vector<Symbol>> M;
public:
    Parser() : M(BuildTable()) {}

    std::unique_ptr<Node> Parse(const std::vector<Token>& toks) const {
        std::vector<Symbol> stack = {Symbol::End, Symbol::I};
        std::vector<std::unique_ptr<Node>> values;
        size_t pos = 0;

        auto pop = [](std::vector<std::unique_ptr<Node>>& values) {
            auto n = std::move(values.back());
            values.pop_back();
            return n;
        };

        while (true) {
            Symbol X = stack.back();
            stack.pop_back();
            const Token& tok = toks[pos];

            if (isTerminal(X)) {
                if (X != static_cast<Symbol>(tok.GetKind())) {
                    if (X == Symbol::RParen) throw ParseError("ожидалась ')'", tok.GetPos());
                    if (X == Symbol::End) throw ParseError("лишний символ '" + tok.ToString() + "'", tok.GetPos());
                    throw ParseError(describe(tok), tok.GetPos());
                }
                if (tok.GetKind() == TokenKind::Var) values.push_back(Node::Var(tok.ToString()));
                if (X == Symbol::End) break;
                ++pos;
            } else if (isAction(X)) {
                if (X == Symbol::MkNot) {
                    values.push_back(Node::Unary(NodeKind::Not, pop(values)));
                } else {
                    auto right = pop(values);
                    auto left = pop(values);
                    values.push_back(Node::Binary(kindOf(X), std::move(left), std::move(right)));
                }
            } else {
                auto it = M.find({X, tok.GetKind()});
                if (it == M.end()) {
                    bool nullable = X == Symbol::I1 || X == Symbol::D1 || X == Symbol::C1;
                    if (nullable) throw ParseError("лишний символ '" + tok.ToString() + "'", tok.GetPos());
                    throw ParseError(describe(tok), tok.GetPos());
                }
                const std::vector<Symbol>& rhs = it->second;
                for (auto r = rhs.rbegin(); r != rhs.rend(); ++r) stack.push_back(*r);
            }
        }
        return pop(values);
    }
};

static Parser defaultParser;
std::unique_ptr<Node> ParseStatement(const std::string& a) {
    return defaultParser.Parse(Tokenize(a));
}

class LogicSystem {
public:
    enum class Id { Classical, Kleene, Lukasiewicz };

private:
    Id id;
    std::string fullName;
    std::string shortName;
    std::vector<double> truthValues;

public:
    LogicSystem(Id id, std::string fullName, std::string shortName, std::vector<double> truthValues)
        : id(id), fullName(std::move(fullName)), shortName(std::move(shortName)),
          truthValues(std::move(truthValues)) {}

    Id GetId() const { return id; }
    const std::string& FullName() const { return fullName; }
    const std::string& ShortName() const { return shortName; }
    const std::vector<double>& TruthValues() const { return truthValues; }
    size_t Arity() const { return truthValues.size(); }

    double Not(double a) const { return 1 - a; }
    double And(double a, double b) const { return std::min(a, b); }
    double Or(double a, double b) const { return std::max(a, b); }
    double Imp(double a, double b) const {
        if (id == Id::Lukasiewicz) return std::min(1.0, 1 - a + b);
        return std::max(1 - a, b);  // C2 и K3
    }
};

const std::vector<LogicSystem>& AllSystems() {
    static const std::vector<LogicSystem> all = {
        LogicSystem(LogicSystem::Id::Classical,   "Классическая двузначная логика (C2)", "C2", {0, 1}),
        LogicSystem(LogicSystem::Id::Kleene,      "Сильная трёхзначная логика Клини (K3)", "K3", {0, 0.5, 1}),
        LogicSystem(LogicSystem::Id::Lukasiewicz, "Трёхзначная логика Лукасевича (L3)", "L3", {0, 0.5, 1}),
    };
    return all;
}

const LogicSystem* FindSystem(const std::string& name) {
    std::string low;
    for (char c : name) low += (char)std::tolower((unsigned char)c);
    for (const auto& sys : AllSystems()) {
        std::string shortLow;
        for (char c : sys.ShortName()) shortLow += (char)std::tolower((unsigned char)c);
        if (low == shortLow) return &sys;
    }
    return nullptr;
}

// -----------------------------------------
// Интерпретации и вычисление значений
// -----------------------------------------

using Interpretation = std::map<std::string, double>;

std::set<std::string> CollectVars(const Node* n) {
    std::set<std::string> vars;
    if (!n) return vars;
    if (n->GetKind() == NodeKind::Var) vars.insert(n->GetName());
    for (const Node* child : {n->GetLeft(), n->GetRight()}) {
        auto sub = CollectVars(child);
        vars.insert(sub.begin(), sub.end());
    }
    return vars;
}

std::vector<Interpretation> AllInterpretations(const std::vector<std::string>& vars,
                                               const std::vector<double>& values) {
    std::vector<Interpretation> out;
    out.reserve(1);
    size_t total = 1;
    for (size_t i = 0; i < vars.size(); ++i) total *= values.size();

    std::vector<size_t> idx(vars.size(), 0);
    for (size_t count = 0; count < total; ++count) {
        Interpretation interp;
        for (size_t i = 0; i < vars.size(); ++i) interp[vars[i]] = values[idx[i]];
        out.push_back(interp);
        for (size_t i = vars.size(); i-- > 0;) {
            if (++idx[i] < values.size()) break;
            idx[i] = 0;
        }
    }
    return out;
}

double Eval(const Node* n, const Interpretation& env, const LogicSystem& sys) {
    switch (n->GetKind()) {
        case NodeKind::Var: return env.at(n->GetName());
        case NodeKind::Not: return sys.Not(Eval(n->GetLeft(), env, sys));
        case NodeKind::And: return sys.And(Eval(n->GetLeft(), env, sys), Eval(n->GetRight(), env, sys));
        case NodeKind::Or:  return sys.Or(Eval(n->GetLeft(), env, sys), Eval(n->GetRight(), env, sys));
        case NodeKind::Imp: return sys.Imp(Eval(n->GetLeft(), env, sys), Eval(n->GetRight(), env, sys));
    }
    return 0;
}

std::string ValStr(double v) {
    if (v == 0) return "0";
    if (v == 1) return "1";
    return "1/2";
}

bool ParseValue(const std::string& s, double& out) {
    if (s == "0") { out = 0; return true; }
    if (s == "1") { out = 1; return true; }
    if (s == "1/2" || s == "0.5") { out = 0.5; return true; }
    return false;
}

std::string InterpStr(const Interpretation& interp) {
    std::ostringstream oss;
    bool first = true;
    for (const auto& [name, value] : interp) {
        if (!first) oss << ", ";
        first = false;
        oss << name << " = " << ValStr(value);
    }
    return oss.str();
}

// -----------------------------------------
// Семантический анализ формул
// -----------------------------------------

struct FormulaAnalysis {
    std::vector<std::string> vars;
    std::vector<std::pair<Interpretation, double>> rows;
    bool tautology = false;
    bool satisfiable = false;
    bool identicallyFalse = false;
    std::vector<size_t> counterexampleRows;
};

FormulaAnalysis Analyze(const Node& formula, const LogicSystem& sys) {
    FormulaAnalysis result;
    auto varSet = CollectVars(&formula);
    result.vars.assign(varSet.begin(), varSet.end());
    auto interps = AllInterpretations(result.vars, sys.TruthValues());

    result.tautology = true;
    result.identicallyFalse = true;
    for (size_t i = 0; i < interps.size(); ++i) {
        double value = Eval(&formula, interps[i], sys);
        result.rows.push_back({interps[i], value});
        if (value != 0) result.identicallyFalse = false;
        if (value != 1) {
            result.tautology = false;
            result.counterexampleRows.push_back(i);
        } else {
            result.satisfiable = true;
        }
    }
    return result;
}

void PrintTruthTable(const Node& formula, const LogicSystem& sys, const FormulaAnalysis& analysis) {
    std::cout << "Таблица истинности в системе " << sys.ShortName() << " ("
              << analysis.rows.size() << " интерпретаций):\n";

    std::vector<size_t> widths;
    std::string sep = "+";
    std::string header = "|";
    for (const auto& v : analysis.vars) {
        size_t w = std::max<size_t>(6, v.size() + 3);
        widths.push_back(w);
        sep += std::string(w, '-') + "+";
        std::ostringstream cell;
        cell << " " << std::setw(w - 2) << std::left << v << " ";
        header += cell.str() + "|";
    }
    size_t fw = std::max<size_t>(10, formula.ToString().size() + 2);
    sep += std::string(fw, '-') + "+";
    std::ostringstream fcell;
    fcell << " " << std::setw(fw - 2) << std::left << formula.ToString() << " ";
    header += fcell.str() + "|";

    std::cout << sep << "\n" << header << "\n" << sep << "\n";
    for (const auto& [interp, value] : analysis.rows) {
        std::cout << "|";
        for (size_t i = 0; i < analysis.vars.size(); ++i) {
            std::ostringstream cell;
            cell << " " << std::setw(widths[i] - 2) << std::right << ValStr(interp.at(analysis.vars[i])) << " ";
            std::cout << cell.str() << "|";
        }
        std::ostringstream cell;
        cell << " " << std::setw(fw - 2) << std::right << ValStr(value) << " ";
        std::cout << cell.str() << "|\n";
    }
    std::cout << sep << "\n";
}

void PrintProperties(const LogicSystem& sys, const FormulaAnalysis& analysis) {
    std::cout << "Свойства формулы в системе " << sys.ShortName() << ":\n";
    std::cout << "  общезначимость (тавтология):       " << (analysis.tautology ? "да" : "нет") << "\n";
    std::cout << "  выполнимость:                      " << (analysis.satisfiable ? "да" : "нет") << "\n";
    std::cout << "  тождественная ложность (всюду 0):  " << (analysis.identicallyFalse ? "да" : "нет") << "\n";
    size_t ce = analysis.counterexampleRows.size();
    if (analysis.tautology) {
        std::cout << "  контрпримеры:                      отсутствуют\n";
    } else {
        std::cout << "  контрпримеры (значение != 1):      " << ce << " шт.\n";
        const size_t maxShown = 10;
        size_t shown = 0;
        for (size_t row : analysis.counterexampleRows) {
            if (shown >= maxShown) {
                std::cout << "    ... и ещё " << (ce - maxShown) << "\n";
                break;
            }
            std::cout << "    " << InterpStr(analysis.rows[row].first)
                      << "  ->  " << ValStr(analysis.rows[row].second) << "\n";
            ++shown;
        }
    }
    if (!analysis.satisfiable && !analysis.identicallyFalse) {
        std::cout << "  примечание: формула невыполнима (не принимает значение 1), но не является\n"
                     "  тождественно ложной — на некоторых интерпретациях её значение равно 1/2.\n";
    }
}

void PrintEvalTrace(const Node* n, const Interpretation& env, const LogicSystem& sys, int depth) {
    if (!n) return;
    PrintEvalTrace(n->GetLeft(), env, sys, depth + 1);
    PrintEvalTrace(n->GetRight(), env, sys, depth + 1);
    std::cout << std::string(depth * 2, ' ') << n->ToString()
              << " = " << ValStr(Eval(n, env, sys)) << "\n";
}

// -----------------------------------------
// Проверка логического следования A1, ..., An |= B (семантический способ)
// -----------------------------------------

enum class ConsequenceStatus { Holds, Fails, InconsistentPremises };

struct ConsequenceResult {
    ConsequenceStatus status = ConsequenceStatus::Holds;
    Interpretation counterexample;
    size_t interpretationsChecked = 0;
    size_t premiseModels = 0;
    double conclusionValue = 0;
};

ConsequenceResult CheckConsequence(const std::vector<const Node*>& premises,
                                   const Node& conclusion,
                                   const LogicSystem& sys) {
    ConsequenceResult result;
    std::set<std::string> varSet;
    for (const Node* p : premises) {
        auto sub = CollectVars(p);
        varSet.insert(sub.begin(), sub.end());
    }
    auto sub = CollectVars(&conclusion);
    varSet.insert(sub.begin(), sub.end());
    std::vector<std::string> vars(varSet.begin(), varSet.end());

    auto interps = AllInterpretations(vars, sys.TruthValues());
    result.interpretationsChecked = interps.size();

    for (const auto& interp : interps) {
        bool allPremisesTrue = true;
        for (const Node* p : premises) {
            if (Eval(p, interp, sys) != 1) { allPremisesTrue = false; break; }
        }
        if (!allPremisesTrue) continue;
        ++result.premiseModels;
        double value = Eval(&conclusion, interp, sys);
        if (value != 1 && result.status != ConsequenceStatus::Fails) {
            result.status = ConsequenceStatus::Fails;
            result.counterexample = interp;
            result.conclusionValue = value;
        }
    }
    if (result.premiseModels == 0) {
        result.status = ConsequenceStatus::InconsistentPremises;
    } else if (result.status != ConsequenceStatus::Fails) {
        result.status = ConsequenceStatus::Holds;
    }
    return result;
}

void PrintConsequence(const std::vector<const Node*>& premises, const Node& conclusion,
                      const LogicSystem& sys, const ConsequenceResult& result) {
    std::ostringstream task;
    for (size_t i = 0; i < premises.size(); ++i) {
        if (i) task << ", ";
        task << premises[i]->ToString();
    }
    task << " |= " << conclusion.ToString();
    std::cout << "Проверка следования в системе " << sys.ShortName() << ": " << task.str() << "\n";
    std::cout << "  перебрано интерпретаций: " << result.interpretationsChecked
              << ", где все посылки равны 1: " << result.premiseModels << "\n";

    switch (result.status) {
        case ConsequenceStatus::Holds:
            std::cout << "  отношение логического следования ВЫПОЛНЯЕТСЯ:\n"
                         "  во всех интерпретациях, где все посылки принимают значение 1,\n"
                         "  вывод также принимает значение 1.\n";
            break;
        case ConsequenceStatus::Fails:
            std::cout << "  отношение логического следования НЕ ВЫПОЛНЯЕТСЯ.\n"
                         "  Контрпример: " << InterpStr(result.counterexample) << "\n"
                         "  Все посылки равны 1, а вывод равен "
                      << ValStr(result.conclusionValue) << " (!= 1).\n";
            break;
        case ConsequenceStatus::InconsistentPremises:
            std::cout << "  ни одна интерпретация не удовлетворяет всем посылкам одновременно\n"
                         "  (посылки несовместны). Отношение следования выполняется вакуумно.\n";
            break;
    }
}

// -----------------------------------------
// Сравнение логических систем
// -----------------------------------------

void CompareSystems(const Node& formula) {
    std::map<std::string, FormulaAnalysis> analyses;
    for (const auto& sys : AllSystems()) analyses[sys.ShortName()] = Analyze(formula, sys);

    std::cout << "Сравнение систем для формулы " << formula.ToString() << ":\n";
    for (const auto& sys : AllSystems()) {
        const auto& a = analyses[sys.ShortName()];
        std::cout << "  " << sys.ShortName() << ": общезначима = " << (a.tautology ? "да" : "нет")
                  << ", выполнима = " << (a.satisfiable ? "да" : "нет")
                  << ", тождественно ложна = " << (a.identicallyFalse ? "да" : "нет") << "\n";
    }

    bool propsCoincide = true;
    for (size_t i = 1; i < AllSystems().size(); ++i) {
        const auto& a = analyses[AllSystems()[i].ShortName()];
        const auto& b = analyses[AllSystems().front().ShortName()];
        if (a.tautology != b.tautology || a.satisfiable != b.satisfiable ||
            a.identicallyFalse != b.identicallyFalse) propsCoincide = false;
    }
    std::cout << "  семантические свойства: " << (propsCoincide ? "совпадают" : "различаются") << "\n";

    auto varSet = CollectVars(&formula);
    std::vector<std::string> vars(varSet.begin(), varSet.end());
    auto common = AllInterpretations(vars, {0, 1});
    bool allAgree = true;
    for (const auto& interp : common) {
        double first = Eval(&formula, interp, AllSystems().front());
        for (size_t i = 1; i < AllSystems().size(); ++i)
            if (Eval(&formula, interp, AllSystems()[i]) != first) allAgree = false;
    }
    std::cout << "  значения на общих интерпретациях {0,1}: "
              << (allAgree ? "совпадают" : "различаются") << "\n";

    const auto& aK3 = analyses["K3"];
    const auto& aL3 = analyses["L3"];
    size_t diff = 0;
    for (size_t i = 0; i < aK3.rows.size() && i < aL3.rows.size(); ++i)
        if (aK3.rows[i].second != aL3.rows[i].second) ++diff;
    if (diff == 0)
        std::cout << "  полные таблицы K3 и L3 совпадают\n";
    else
        std::cout << "  полные таблицы K3 и L3 различаются (в " << diff << " из "
                  << aK3.rows.size() << " строк)\n";
}

// -----------------------------------------
// Интерфейс командной строки
// -----------------------------------------

std::string Trim(const std::string& s) {
    size_t b = s.find_first_not_of(" \t");
    if (b == std::string::npos) return "";
    size_t e = s.find_last_not_of(" \t");
    return s.substr(b, e - b + 1);
}

std::vector<std::string> SplitWS(const std::string& s) {
    std::vector<std::string> out;
    std::istringstream iss(s);
    std::string word;
    while (iss >> word) out.push_back(word);
    return out;
}

void PrintHelp() {
    std::cout <<
        "Доступные команды:\n"
        "  eval <система> <формула> [переменная=значение ...]\n"
        "      Вычислить формулу в системе C2, K3 или L3.\n"
        "      Без оценок — строится полная таблица истинности и проверяются\n"
        "      семантические свойства; с оценками (p=1 q=1/2) — значение формулы\n"
        "      на одной интерпретации с разбором подформул.\n"
        "  entail <система> <посылка1, посылка2, ...> |= <вывод>\n"
        "      Проверка отношения логического следования A1, ..., An |= B.\n"
        "  compare <формула>\n"
        "      Интерпретация формулы сразу во всех системах со сравнением результатов.\n"
        "  systems\n"
        "      Показать семантику логических операций в каждой системе.\n"
        "  demo\n"
        "      Проверка работоспособности на формулах варианта 4.\n"
        "  help — эта справка;  exit — выход.\n"
        "Операции: ~, &, |, -> (допускаются также ¬, ∧, ∨, →). Значения: 0, 1/2, 1.\n";
}

void PrintSystemSemantics() {
    for (const auto& sys : AllSystems()) {
        std::cout << sys.FullName() << "\n";
        std::cout << "  множество значений: {";
        for (size_t i = 0; i < sys.TruthValues().size(); ++i) {
            if (i) std::cout << ", ";
            std::cout << ValStr(sys.TruthValues()[i]);
        }
        std::cout << "}\n";
        std::cout << "  ~a = 1 - a;  a & b = min(a, b);  a | b = max(a, b)\n";
        if (sys.GetId() == LogicSystem::Id::Lukasiewicz)
            std::cout << "  a -> b = min(1, 1 - a + b)\n";
        else
            std::cout << "  a -> b = max(1 - a, b)\n";
        const auto& vals = sys.TruthValues();
        std::cout << "  таблица импликации:\n";
        std::cout << "+-------+";
        for (size_t i = 0; i < vals.size(); ++i) std::cout << "-------+";
        std::cout << "\n|       |";
        for (double b : vals) std::cout << std::setw(6) << ValStr(b) << " |";
        std::cout << "\n";
        std::cout << "+-------+";
        for (size_t i = 0; i < vals.size(); ++i) std::cout << "-------+";
        std::cout << "\n";
        for (double a : vals) {
            std::cout << "|" << std::setw(6) << ValStr(a) << " |";
            for (double b : vals) std::cout << std::setw(6) << ValStr(sys.Imp(a, b)) << " |";
            std::cout << "\n";
        }
        std::cout << "+-------+";
        for (size_t i = 0; i < vals.size(); ++i) std::cout << "-------+";
        std::cout << "\n";
    }
}

const LogicSystem* RequireSystem(const std::vector<std::string>& words, size_t idx) {
    if (idx >= words.size()) throw std::runtime_error("не указана логическая система (C2, K3 или L3)");
    const LogicSystem* sys = FindSystem(words[idx]);
    if (!sys) throw std::runtime_error("неизвестная система '" + words[idx] + "' (C2, K3 или L3)");
    return sys;
}

std::unique_ptr<Node> RequireFormula(const std::string& text, const std::string& what) {
    std::string trimmed = Trim(text);
    if (trimmed.empty()) throw std::runtime_error("не указана " + what);
    try {
        return ParseStatement(trimmed);
    } catch (const ParseError& e) {
        std::ostringstream oss;
        oss << "синтаксическая ошибка в " << what << ": " << e.what() << " (позиция " << e.pos << ")";
        throw std::runtime_error(oss.str());
    }
}

// eval <система> <формула> [x=значение ...]
void CommandEval(const std::vector<std::string>& words) {
    const LogicSystem* sys = RequireSystem(words, 1);
    if (words.size() < 3) throw std::runtime_error("не указана формула");

    size_t split = words.size();
    while (split > 2 && words[split - 1].find('=') != std::string::npos) --split;
    std::string formulaText;
    for (size_t i = 2; i < split; ++i) {
        if (i > 2) formulaText += " ";
        formulaText += words[i];
    }
    auto formula = RequireFormula(formulaText, "формуле");

    if (split == words.size()) {
        std::cout << "Формула: " << formula->ToString() << "\n";
        auto analysis = Analyze(*formula, *sys);
        PrintTruthTable(*formula, *sys, analysis);
        PrintProperties(*sys, analysis);
        return;
    }

    Interpretation env;
    for (size_t i = split; i < words.size(); ++i) {
        size_t eq = words[i].find('=');
        std::string name = words[i].substr(0, eq);
        std::string valStr = words[i].substr(eq + 1);
        double value;
        if (name.empty() || !ParseValue(valStr, value))
            throw std::runtime_error("некорректная оценка '" + words[i] + "' (формат: p=1, q=1/2)");
        if (std::find(sys->TruthValues().begin(), sys->TruthValues().end(), value) == sys->TruthValues().end())
            throw std::runtime_error("значение " + valStr + " недопустимо в системе " + sys->ShortName());
        env[name] = value;
    }
    auto varSet = CollectVars(formula.get());
    std::vector<std::string> missing;
    for (const auto& v : varSet)
        if (env.find(v) == env.end()) missing.push_back(v);
    if (!missing.empty()) {
        std::ostringstream oss;
        oss << "не заданы оценки для переменных:";
        for (const auto& v : missing) oss << " " << v;
        throw std::runtime_error(oss.str());
    }

    std::cout << "Формула: " << formula->ToString() << "\n";
    std::cout << "Система: " << sys->FullName() << "\n";
    std::cout << "Интерпретация: " << InterpStr(env) << "\n";
    std::cout << "Значения подформул (от внутренних к внешним):\n";
    PrintEvalTrace(formula.get(), env, *sys, 0);
}

// entail <система> <посылки> |= <вывод>
void CommandEntail(const std::vector<std::string>& words) {
    const LogicSystem* sys = RequireSystem(words, 1);
    size_t sep = words.size();
    for (size_t i = 2; i < words.size(); ++i)
        if (words[i] == "|=") { sep = i; break; }
    if (sep == words.size()) throw std::runtime_error("отсутствует разделитель '|='");
    if (sep == 2) throw std::runtime_error("не указаны посылки");
    if (sep + 1 >= words.size()) throw std::runtime_error("не указан вывод");

    std::string premisesText;
    for (size_t i = 2; i < sep; ++i) {
        if (i > 2) premisesText += " ";
        premisesText += words[i];
    }
    std::string conclusionText;
    for (size_t i = sep + 1; i < words.size(); ++i) {
        if (i > sep + 1) conclusionText += " ";
        conclusionText += words[i];
    }

    std::vector<std::unique_ptr<Node>> premiseNodes;
    std::vector<const Node*> premises;
    std::string rest = premisesText;
    while (true) {
        size_t comma = rest.find(',');
        std::string piece = Trim(comma == std::string::npos ? rest : rest.substr(0, comma));
        if (!piece.empty()) {
            premiseNodes.push_back(RequireFormula(piece, "посылке"));
            premises.push_back(premiseNodes.back().get());
        }
        if (comma == std::string::npos) break;
        rest = rest.substr(comma + 1);
    }
    if (premises.empty()) throw std::runtime_error("не указаны посылки");
    auto conclusion = RequireFormula(conclusionText, "выводе");

    auto result = CheckConsequence(premises, *conclusion, *sys);
    PrintConsequence(premises, *conclusion, *sys, result);
}

// compare <формула>
void CommandCompare(const std::vector<std::string>& words) {
    if (words.size() < 2) throw std::runtime_error("не указана формула");
    std::string formulaText;
    for (size_t i = 1; i < words.size(); ++i) {
        if (i > 1) formulaText += " ";
        formulaText += words[i];
    }
    auto formula = RequireFormula(formulaText, "формуле");
    for (const auto& sys : AllSystems()) {
        auto analysis = Analyze(*formula, sys);
        PrintTruthTable(*formula, sys, analysis);
    }
    CompareSystems(*formula);
}

void CommandDemo() {
    const std::vector<std::string> formulas = {
        "p -> p",
        "(p | ~p) -> (q | ~q)",
        "(p | q) -> (p & q)",
        "(p -> (q -> r)) -> ((p -> q) -> (p -> r))",
        "(p & ~p) | (q & ~q)",
    };
    const std::vector<std::pair<std::vector<std::string>, std::string>> relations = {
        {{"p", "p -> (q | r)", "q -> s", "r -> t", "(s | t) -> u", "u -> v"}, "u"},
        {{"p -> (q & r)", "q -> s", "r -> t", "(s & t) -> u", "u -> v"}, "~p | u"},
        {{"(p & q) | r", "p -> s", "r -> s", "s -> t", "t -> u"}, "u & (q | ~q)"},
        {{"p | q", "p -> r", "q -> s", "(r & s) -> t", "t -> u"}, "u"},
    };

    std::cout << "================ Задание 1: исследование формул ================\n";
    for (size_t i = 0; i < formulas.size(); ++i) {
        auto formula = ParseStatement(formulas[i]);
        std::cout << "\n### F" << (i + 1) << " = " << formula->ToString() << "\n";
        for (const auto& sys : AllSystems()) {
            auto analysis = Analyze(*formula, sys);
            PrintTruthTable(*formula, sys, analysis);
            PrintProperties(sys, analysis);
        }
        CompareSystems(*formula);
    }

    std::cout << "\n================ Задание 2: логическое следование ================\n";
    for (size_t i = 0; i < relations.size(); ++i) {
        std::vector<std::unique_ptr<Node>> premiseNodes;
        std::vector<const Node*> premises;
        for (const auto& text : relations[i].first) {
            premiseNodes.push_back(ParseStatement(text));
            premises.push_back(premiseNodes.back().get());
        }
        auto conclusion = ParseStatement(relations[i].second);
        std::cout << "\n### R" << (i + 1) << "\n";
        for (const auto& sys : AllSystems()) {
            auto result = CheckConsequence(premises, *conclusion, sys);
            PrintConsequence(premises, *conclusion, sys, result);
        }
    }
}

void REPL() {
    PrintHelp();
    std::string line;
    while (true) {
        std::cout << ">>> ";
        if (!std::getline(std::cin, line)) break;
        std::vector<std::string> words = SplitWS(line);
        if (words.empty()) continue;
        const std::string& cmd = words[0];
        try {
            if (cmd == "exit" || cmd == "quit") break;
            else if (cmd == "help") PrintHelp();
            else if (cmd == "eval") CommandEval(words);
            else if (cmd == "entail") CommandEntail(words);
            else if (cmd == "compare") CommandCompare(words);
            else if (cmd == "systems") PrintSystemSemantics();
            else if (cmd == "demo") CommandDemo();
            else std::cout << "Неизвестная команда '" << cmd << "' (help — справка).\n";
        } catch (const ParseError& e) {
            std::cout << "Ошибка в формуле: " << e.what() << " (позиция " << e.pos << ")\n";
        } catch (const std::runtime_error& e) {
            std::cout << "Ошибка: " << e.what() << "\n";
        }
    }
}

int main() {
    std::cout << "Лабораторная работа №2. Интерпретатор многозначных логик (C2, K3, L3).\n";
    REPL();
    return 0;
}
