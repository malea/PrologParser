% Representation of grammar. Nonterminals expr, term, term_tail, 
% and factor_tail are represented as non(e,_), non(t,_), non(tt,_), 
% and non(ft,_), respectively. Special nonterminal start is encoded
% as non(s,_).
% Terminals num, -, and * are represented by term(num,_), term(minus,_)
% and term(times,_). Special terminal term(eps,_) denotes the epsilon
% symbol.
% 
% Productions are represented with prod(N,[H|T]) --- that is, arguments 
% are the production index N and a list [H|T] where the head of the 
% list H is the left-hand-side of the production, and the tail of
% the list T is the right-hand-side of the production. For example,
% production expr -> term term_tail is represented as
% prod(1,[non(e,_),non(t,_),non(tt,_)]).

prod(0,[non(s,_),non(e,_)]).
prod(1,[non(e,_),non(t,_),non(tt,_)]). 
prod(2,[non(tt,_),term(minus,_),non(t,_),non(tt,_)]).
prod(3,[non(tt,_),term(eps,_)]).
prod(4,[non(t,_),term(num,_),non(ft,_)]).
prod(5,[non(ft,_),term(times,_),term(num,_),non(ft,_)]).
prod(6,[non(ft,_),term(eps,_)]).

% LL(1) Parsing table.
% predict(non(s,_),term(num,_),0) stands for "on start and num, predict 
% production 0. start -> expr"
% predict(non(e,_),term(num,_),1) stands for "on nonterminal expr and
% terminal num, predict production 1. expr -> term term_tail".

% YOUR CODE HERE. 
% Complete the LL(1) parsing table for the grammar.
predict(non(s,_),term(num,_),0).
predict(non(e,_),term(num,_),1).
predict(non(tt,_),term(minus,_),2).
predict(non(tt,_),term(dollar,_),3).
predict(non(t,_),term(num,_),4).
predict(non(ft,_),term(minus,_),6).
predict(non(ft,_),term(times,_),5).
predict(non(ft,_),term(dollar,_),6).

% sample inputs
input0([3,-,5]).
input1([3,-,5,*,7,-,18]).


% YOUR CODE HERE.
% Write transform(L,R): it takes input list L and transforms it into a
% list where terminals are represented with term(...). The transformed 
% list will be computed in unbound variable R.
% E.g., transform([3,-,5],R).
% R = [term(num,3),term(minus,_),term(num,5)]
transformhelper(Token,Terminal) :-
    (number(Token), Terminal = term(num,Token));
    (Token == -, Terminal = term(minus,_));
    (Token == *, Terminal = term(times,_));
    (Token == eps, Terminal = term(eps,_)).

transform([],[term(dollar,_)]).
transform([ TokenHead | TokenTail ], [ TerminalHead | TerminalTail ]) :-
    transformhelper(TokenHead, TerminalHead),
    transform(TokenTail,TerminalTail).

% You will write parseLL(L,ProdSeq): it will take a transformed 
% list R and will produce the production sequence applied by 
% the predictive parser.
% E.g., input0(L),transform(L,R),parseLL(R,ProdSeq).
% ProdSeq = [0, 1, 4, 6, 2, 4, 6, 3].

parseHelper([],[],[]).
parseHelper(Stack, Input, ProdSeq) :-
    Stack = [StackHead | StackTail],
    Input = [InputHead | InputTail],
    (
        % consume tokens when the input and stack match
        ( StackHead = InputHead, parseHelper(StackTail, InputTail, ProdSeq) );
 
        % epsilons should disappear from the stack without changing input
        % (don't remove InputHead)
        ( StackHead = term(eps,_), parseHelper(StackTail, Input, ProdSeq) );
 
        % otherwise, recursively parse the rest
        (
            % predict next production using the stack head and input head
            predict(StackHead, InputHead, ProdNumber),
            % look up the expansion of the production
            prod(ProdNumber, [_ | Expansion]),
            % append the expansion to the end of the stack
            append(Expansion, StackTail, NewStack),
            % get the results of the rest of the parse as PrevProdSeq
            parseHelper(NewStack, Input, PrevProdSeq),
            % append the current production to the recursive results list to get the
            % final list
            append([ProdNumber],PrevProdSeq,ProdSeq)
        )
    ).
 
% take user input, tokenize, and parse!
parseLL(Input,ProdSeq) :-
    StartSymbol = non(s,_),
    EndOfStack = term(dollar,_),
    InitialStack = [StartSymbol, EndOfStack],
    parseHelper(InitialStack,Input,ProdSeq).

% Later you will agument parseLL with the computation of
% the expression value.
% E.g., input0(L),transform(L,R),parseLL(R,ProdSeq,V).
% ProdSeq = [0, 1, 4, 6, 2, 4, 6, 3],
% V = -2.
