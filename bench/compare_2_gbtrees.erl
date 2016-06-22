%% Copyright (c) 2016
%% redink <cnredink@gmail.com>
%%
%% Permission is  hereby  granted,  free of charge,  to any person
%% obtaining  a copy of this software and associated documentation
%% files (the "Software"),to deal in the Software without restric-
%% tion,  including  without  limitation the rights to use,  copy,
%% modify, merge,  publish,  distribute,  sublicense,  and/or sell
%% copies  of the  Software,  and to  permit  persons to  whom the
%% Software  is  furnished  to do  so,  subject  to the  following
%% conditions:
%%
%% The above  copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF  MERCHANTABILITY,  FITNESS  FOR  A  PARTICULAR  PURPOSE  AND
%% NONINFRINGEMENT. IN  NO  EVENT  SHALL  THE AUTHORS OR COPYRIGHT
%% HOLDERS  BE  LIABLE FOR  ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT,  TORT  OR OTHERWISE,  ARISING
%% FROM,  OUT OF OR IN CONNECTION WITH THE SOFTWARE  OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.

-module(compare_2_gbtrees).

-export([start/0, insert/2]).

start() ->
    L1 = [{X, a} || X <- lists:seq(1, 10000)],
    {T1, Tree1} = timer:tc(avltree, new, [L1]),
    {T2, Tree2} = timer:tc(?MODULE, insert, [gb_trees:empty(), L1]),
    A1 =
        [begin
            X = rand:uniform(10000),
            {X1, _} = timer:tc(avltree, look, [Tree1, X]),
            X1
         end || _ <- lists:seq(1, 100000)],
    A2 =
        [begin
            X = rand:uniform(10000),
            {X2, _} = timer:tc(gb_trees, lookup, [X, Tree2]),
            X2
         end || _ <- lists:seq(1, 100000)],
    {{look, lists:sum(A1) / lists:sum(A2)},
     {insert, T1 / T2}}.

insert(T, []) ->
    T;
insert(T, [{K, V} | Tail]) ->
    insert(gb_trees:insert(K, V, T), Tail).
