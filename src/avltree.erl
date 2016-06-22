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

-module(avltree).

-export([ new/0
        , new/1
        , insert/2
        , delete/2
        , look/2
        , last/1
        , first/1
        , prev/2
        , next/2
        , prev_traverse/1
        , mid_traverse/1
        , last_traverse/1
        , is_avl/1
        ]).

%%%*_ MACROS and SPECS =========================================================

-define(undef, undefined).
-define(empty_tree,
        #node{root = ?undef, left = ?undef, right = ?undef,
              rootvalue = ?undef, height = 0}).

-record(node, {root, left, right, rootvalue, height = 0}).

-type tree() :: #node{} | ?undef.

%%%*_ API FUNCTIONS ============================================================

-spec new() -> tree().
new() ->
    ?empty_tree.

-spec new([{term(), term()}]) -> tree().
new(KVList) ->
    lists:foldl(fun({Key, Value}, Acc) -> insert(Acc, {Key, Value}) end,
                new(), KVList).

-spec insert(tree(), {term(), term()}) -> tree().
insert(OldTree, {Key, Value}) when OldTree == ?empty_tree; OldTree == ?undef ->
    #node{root = Key, rootvalue = Value, height = 1};
insert(#node{ root = Root
            , left = Left
            , right = Right} = OldTree,
       {Key, Value}) ->
    if
        Key == Root ->
            OldTree#node{rootvalue = Value};
        Key < Root ->
            update_tree(OldTree#node{left = insert(Left, {Key, Value})},
                        Key, insert_left);
        Key > Root ->
            update_tree(OldTree#node{right = insert(Right, {Key, Value})},
                        Key, insert_right)
    end.

-spec delete(tree(), term()) -> tree().
delete(T, _) when T == ?empty_tree; T == ?undef -> T;
delete(#node{ root = Root
            , left = Left
            , right = Right
            } = OldTree,
       Key) ->
    if
        Key == Root ->
            case Right of
                ?undef -> Left;
                _      ->
                    {NewRoot, NewRootValue} = get_min(Right),
                    update_tree(OldTree#node{ root = NewRoot
                                            , right = deletemin(Right)
                                            , rootvalue = NewRootValue},
                                delete_right)
            end;
        Key > Root ->
            update_tree(OldTree#node{right = delete(Right, Key)},
                        delete_right);
        Key < Root ->
            update_tree(OldTree#node{left = delete(Left, Key)},
                        delete_left)
    end.

-spec look(tree(), term()) -> [{term(), term()}].
look(T, _) when T == ?empty_tree; T == ?undef -> [];
look(#node{ root = Root
          , left = Left
          , right = Right
          , rootvalue = RootValue}, Key) ->
    if
        Key == Root ->
            [{Key, RootValue}];
        Key > Root ->
            look(Right, Key);
        Key < Root ->
            look(Left, Key)
    end.

-spec last(tree()) -> '$end_of_tree' | term().
last(T) when T == ?empty_tree; T == ?undef -> '$end_of_tree';
last(#node{root = Root, right = ?undef})   -> Root;
last(#node{right = Right})                 -> last(Right).

-spec first(tree()) -> '$end_of_tree' | term().
first(T) when T == ?empty_tree; T == ?undef -> '$end_of_tree';
first(#node{root = Root, left = ?undef})    -> Root;
first(#node{left = Left})                   -> first(Left).

-spec next(tree(), term()) -> '$end_of_tree' | {term(), term()}.
next(T, _) when T == ?empty_tree; T == ?undef -> '$end_of_tree';
next(#node{ root = Root
          , left = Left
          , right = Right
          , rootvalue = RootValue}, Key) ->
    if
        Key == Root ->
            {Root, RootValue};
        Key > Root ->
            next(Right, Key);
        Key < Root andalso Left == ?undef ->
            {Root, RootValue};
        Key < Root ->
            case last(Left) < Key of
                true ->
                    {Root, RootValue};
                false ->
                    next(Left, Key)
            end
    end.

-spec prev(tree(), term()) -> '$end_of_tree' | {term(), term()}.
prev(T, _) when T == ?empty_tree; T == ?undef -> '$end_of_tree';
prev(#node{ root = Root
          , left = Left
          , right = Right
          , rootvalue = RootValue}, Key) ->
    if
        Key == Root ->
            {Root, RootValue};
        Key < Root ->
            prev(Left, Key);
        Key > Root andalso Right == ?undef ->
            {Root, RootValue};
        Key > Root ->
            case first(Right) > Key of
                true ->
                    {Root, RootValue};
                false ->
                    prev(Right, Key)
            end
    end.

-spec prev_traverse(tree()) -> [term()].
prev_traverse(?undef) -> [];
prev_traverse(#node{root = Root, left = ?undef, right = ?undef}) ->
    [Root];
prev_traverse(#node{root = Root, left = ?undef, right = Right}) ->
    [Root | prev_traverse(Right)];
prev_traverse(#node{root = Root, left = Left, right = ?undef}) ->
    [Root | prev_traverse(Left)];
prev_traverse(#node{root = Root, left = Left, right = Right}) ->
    [Root] ++ prev_traverse(Left) ++ prev_traverse(Right).

-spec mid_traverse(tree()) -> [term()].
mid_traverse(?undef) -> [];
mid_traverse(#node{root = Root, left = Left, right = Right}) ->
    case {Left, Right} of
        {?undef, ?undef} ->
            [Root];
        {?undef, _} ->
            [Root | mid_traverse(Right)];
        {_, ?undef} ->
            mid_traverse(Left) ++ [Root];
        _ ->
            mid_traverse(Left) ++ [Root] ++ mid_traverse(Right)
    end.

-spec last_traverse(tree()) -> [term()].
last_traverse(?undef) -> [];
last_traverse(#node{root = Root, left = Left, right = Right}) ->
    case {Left, Right} of
        {?undef, ?undef} ->
            [Root];
        {?undef, _} ->
            last_traverse(Right) ++ [Root];
        {_, ?undef} ->
            last_traverse(Left) ++ [Root];
        _ ->
            last_traverse(Left) ++ last_traverse(Right) ++ [Root]
    end.

-spec is_avl(tree()) -> boolean().
is_avl(T) when T == ?empty_tree; T == ?undef -> true;
is_avl(#node{left = Left, right = Right} = Tree) ->
    is_binarysearchtree(Tree)
        andalso erlang:abs(height(Left) - height(Right)) =< 1
        andalso is_avl(Left) andalso is_avl(Right).

%%%*_ PRIVATE FUNCTIONS ========================================================

update_tree(#node{ left = Left
                 , right = Right
                 } = Tree,
            Key, insert_left) ->
    case height(Left) - height(Right) == 2 of
        true ->
            if
                Key < Left#node.root ->
                    left_left_rotation(Tree);
                true ->
                    left_right_rotation(Tree) 
            end;
        false ->
            update_height(Tree)
    end;
update_tree(#node{ left = Left
                 , right = Right} = Tree,
            Key, insert_right) ->
    case height(Right) - height(Left) == 2 of
        true ->
            if
                Key > Right#node.root ->
                    right_right_rotation(Tree);
                true ->
                    right_left_rotation(Tree)
            end;
        false ->
            update_height(Tree)
    end.

get_min(#node{ root = Root
             , left = ?undef
             , rootvalue = RootValue}) ->
    {Root, RootValue};
get_min(#node{left = Left}) ->
    get_min(Left).

deletemin(?empty_tree = T) -> T;
deletemin(#node{left = ?undef, right = Right}) -> Right;
deletemin(#node{left = Left} = OldTree) ->
    case Left#node.left of
        ?undef ->
            %% found min
            update_tree(OldTree#node{left = Left#node.right}, delete_left);
        _ ->
            update_tree(OldTree#node{left = deletemin(Left)}, delete_left)
    end.

update_tree(#node{ left = Left
                 , right = Right
                 } = Tree,
            delete_left) ->
    case height(Right) - height(Left) == 2 of
        true ->
            case height(Right#node.left) > height(Right#node.right) of
                true ->
                    right_left_rotation(Tree);
                false ->
                    right_right_rotation(Tree)
            end;
        false ->
            update_height(Tree)
    end;
update_tree(#node{ left = Left
                 , right = Right
                 } = Tree,
            delete_right) ->
    case height(Left) - height(Right) == 2 of
        true ->
            case height(Left#node.right) > height(Left#node.left) of
                true ->
                    left_right_rotation(Tree);
                false ->
                    left_left_rotation(Tree)
            end;
        false ->
            update_height(Tree)
    end.

%%
%%     4             4
%%    / \  insert   / \     LL
%%   2   5 =====>  2   5  ======>  2
%%  / \           / \             / \
%% 1   3         1   3           1   4
%%              /               /   / \
%%             0               0   3   5
%%
left_left_rotation(#node{left = OldLeft} = OldTree) ->
    NewRight = update_height(OldTree#node{left = OldLeft#node.right}),
    update_height(OldLeft#node{right = NewRight}).

%%
%%     1              1
%%    / \   insert   / \     RR
%%   0   3  =====>  0   3  ======>   3
%%      / \            / \          / \
%%     2  4           2   4        1   4
%%                         \      / \   \
%%                          5    0   2   5
%%
right_right_rotation(#node{right = OldRight} = OldTree) ->
    NewLeft = update_height(OldTree#node{right = OldRight#node.left}),
    update_height(OldRight#node{left = NewLeft}).

%%
%%     2              2               2
%%    / \  insert    / \     LL      / \    RR
%%   0   10 =====>  0  10  ======>  0   3  ====>    3
%%       / \           / \               \         / \
%%      3  11         3  11              10       2  10
%%                     \                 / \     /   / \
%%                      9               9   11  0   9  11
%%
right_left_rotation(#node{right = OldRight} = OldTree) ->
    NewRight = left_left_rotation(OldRight),
    right_right_rotation(OldTree#node{right = NewRight}).

%%
%%     8             8               8            7
%%    / \  insert   / \     RR      / \    LL    / \
%%   5   9 =====>  5   9  ======>  7   9  ====> 5   8
%%  / \           / \             /            / \   \
%% 3   7         3   7           5            3   6   9
%%                  /           / \
%%                 6           3   6
%%
left_right_rotation(#node{left = OldLeft} = OldTree) ->
    NewLeft = right_right_rotation(OldLeft),
    left_left_rotation(OldTree#node{left = NewLeft}).

update_height(#node{left = Left, right = Right} = Tree) ->
    Height = erlang:max(height(Left), height(Right)) + 1,
    Tree#node{height = Height}.

height(?undef) -> 0;
height(#node{height = Height}) -> Height.

is_binarysearchtree(T) when T == ?undef; T == ?empty_tree -> true;
is_binarysearchtree(#node{left = Left,
                          right = Right, root = Root}) ->
    case {Left, Right} of
        {?undef, ?undef} ->
            true;
        {?undef, _} ->
            Root < Right#node.root andalso is_binarysearchtree(Right);
        {_, ?undef} ->
            Root > Left#node.root andalso is_binarysearchtree(Left);
        _ ->
            Root > Left#node.root andalso Root < Right#node.root
                andalso is_binarysearchtree(Left)
                andalso is_binarysearchtree(Right)
    end.

%%%*_ TESTS ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

avltree_test_() ->
    [ {"insert", timeout, 10,
        fun() ->
            [lists:foldl(fun({Key, Value}, Acc) ->
                             X = insert(Acc, {Key, Value}),
                             ?assertEqual(true, is_avl(X)),
                             X
                         end,
                         new(),
                         [begin T = rand:uniform(16), {T, T} end
                          || _ <- lists:seq(1, 100)])
             || _ <- lists:seq(1, 100)]
        end}
    , {"prev/mid/last",
        fun() ->
            A = [{3,a}, {2,a}, {1,a}, {4,a}, {5,a}, {6,a}, {7,a}, {16,a},
                 {15,a}, {14,a}, {13,a}, {12,a}, {11,a}, {10,a}, {8,a}, {9,a}],
            Tree = new(A),
            ?assertEqual([7,4,2,1,3,6,5,13,11,9,8,10,12,15,14,16], prev_traverse(Tree)),
            ?assertEqual([1,3,2,5,6,4,8,10,9,12,11,14,16,15,13,7], last_traverse(Tree)),
            ?assertEqual([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16], mid_traverse(Tree))
        end}
    , {"deletemin",
        fun() ->
            A = [{3,a}, {2,a}, {1,a}, {4,a}, {5,a}, {6,a}, {7,a}, {16,a},
                 {15,a}, {14,a}, {13,a}, {12,a}, {11,a}, {10,a}, {8,a}, {9,a}],
            Tree = T00 = new(A),
            T01 = deletemin(T00), ?assertEqual(true, is_avl(T01)),
            T02 = deletemin(T01), ?assertEqual(true, is_avl(T02)),
            T03 = deletemin(T02), ?assertEqual(true, is_avl(T03)),
            T04 = deletemin(T03), ?assertEqual(true, is_avl(T04)),
            T05 = deletemin(T04), ?assertEqual(true, is_avl(T05)),
            T06 = deletemin(T05), ?assertEqual(true, is_avl(T06)),
            T07 = deletemin(T06), ?assertEqual(true, is_avl(T07)),
            T08 = deletemin(T07), ?assertEqual(true, is_avl(T08)),
            T09 = deletemin(T08), ?assertEqual(true, is_avl(T09)),
            T10 = deletemin(T09), ?assertEqual(true, is_avl(T10)),
            T11 = deletemin(T10), ?assertEqual(true, is_avl(T11)),
            T12 = deletemin(T11), ?assertEqual(true, is_avl(T12)),
            T13 = deletemin(T12), ?assertEqual(true, is_avl(T13)),
            T14 = deletemin(T13), ?assertEqual(true, is_avl(T14)),
            T15 = deletemin(T14), ?assertEqual(true, is_avl(T15)),
            T16 = deletemin(T15), ?assertEqual(true, is_avl(T16)),
            ?assertEqual(mid_traverse(T01), lists:nthtail(1 , mid_traverse(Tree))),
            ?assertEqual(mid_traverse(T02), lists:nthtail(2 , mid_traverse(Tree))),
            ?assertEqual(mid_traverse(T03), lists:nthtail(3 , mid_traverse(Tree))),
            ?assertEqual(mid_traverse(T04), lists:nthtail(4 , mid_traverse(Tree))),
            ?assertEqual(mid_traverse(T05), lists:nthtail(5 , mid_traverse(Tree))),
            ?assertEqual(mid_traverse(T06), lists:nthtail(6 , mid_traverse(Tree))),
            ?assertEqual(mid_traverse(T07), lists:nthtail(7 , mid_traverse(Tree))),
            ?assertEqual(mid_traverse(T08), lists:nthtail(8 , mid_traverse(Tree))),
            ?assertEqual(mid_traverse(T09), lists:nthtail(9 , mid_traverse(Tree))),
            ?assertEqual(mid_traverse(T10), lists:nthtail(10, mid_traverse(Tree))),
            ?assertEqual(mid_traverse(T11), lists:nthtail(11, mid_traverse(Tree))),
            ?assertEqual(mid_traverse(T12), lists:nthtail(12, mid_traverse(Tree))),
            ?assertEqual(mid_traverse(T13), lists:nthtail(13, mid_traverse(Tree))),
            ?assertEqual(mid_traverse(T14), lists:nthtail(14, mid_traverse(Tree))),
            ?assertEqual(mid_traverse(T15), lists:nthtail(15, mid_traverse(Tree))),
         ?assertEqual(mid_traverse(T16), lists:nthtail(16, mid_traverse(Tree)))
        end}
    , {"delete",
        fun() ->
            A = [{3,a}, {2,a}, {1,a}, {4,a}, {5,a}, {6,a}, {7,a}, {16,a},
                 {15,a}, {14,a}, {13,a}, {12,a}, {11,a}, {10,a}, {8,a}, {9,a}],
            TT00 = new(A),
            [lists:foldl(fun(K, T) ->
                             NewT = delete(T, K),
                             ?assertEqual(true, is_avl(NewT)),
                             NewT
                         end,
                         TT00, [rand:uniform(16) || _ <- lists:seq(1, 100)])
             || _ <- lists:seq(1, 1000)]
        end}
    , {"look/2",
        fun() ->
            A = [{3,a}, {2,a}, {1,a}, {4,a}, {5,a}, {6,a}, {7,a}, {16,a},
                 {15,a}, {14,a}, {13,a}, {12,a}, {11,a}, {10,a}, {8,a}, {9,a}],
            Tree = new(A),
            [?assertEqual([{X, a}], look(Tree, X)) || X <- lists:seq(1, 16)],
            ?assertEqual([], look(Tree, 0)),
            ?assertEqual([], look(?empty_tree, 0)),
            ?assertEqual([], look(?undef, 0))
        end}
    , {"first/1",
        fun() ->
            A = [{3,a}, {2,a}, {1,a}, {4,a}, {5,a}, {6,a}, {7,a}, {16,a},
                 {15,a}, {14,a}, {13,a}, {12,a}, {11,a}, {10,a}, {8,a}, {9,a}],
            Tree = new(A),
            ?assertEqual(1, first(Tree)),
            ?assertEqual('$end_of_tree', first(?undef)),
            ?assertEqual('$end_of_tree', first(?empty_tree))
        end}
    , {"last/1",
        fun() ->
            A = [{3,a}, {2,a}, {1,a}, {4,a}, {5,a}, {6,a}, {7,a}, {16,a},
                 {15,a}, {14,a}, {13,a}, {12,a}, {11,a}, {10,a}, {8,a}, {9,a}],
            Tree = new(A),
            ?assertEqual(16, last(Tree)),
            ?assertEqual('$end_of_tree', last(?undef)),
            ?assertEqual('$end_of_tree', last(?empty_tree))
        end}
    , {"next/2",
        fun() ->
            A = [{3,a}, {2,a}, {1,a}, {4,a}, {5,a}, {6,a}, {7,a}, {16,a},
                 {15,a}, {14,a}, {13,a}, {12,a}, {11,a}, {10,a}, {8,a}, {9,a}],
            Tree = new(A),
            ?assertEqual({1, a}, next(Tree, 0)),
            ?assertEqual({1, a}, next(Tree, 1)),
            ?assertEqual('$end_of_tree', next(Tree, 17))
        end}
  ].

-endif.
