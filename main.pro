% furkan bulbul
% 2020400318
% compiling: yes
% complete: yes
:- ['cmpecraft.pro'].

:- init_from_map.

%*************************************************
% 10 points
% manhattan_distance(+A, +B, -Distance) :- .
%*************************************************

%calculates the manhattan distance between two points 
manhattan_distance(A, B, Distance) :-
    A = [X1, Y1],
    B = [X2, Y2],
    Distance is abs(X1-X2) + abs(Y1-Y2).

%*************************************************
% 10 points
% minimum_of_list(+List, -Minimum) :- .
%*************************************************

%predicates to calculate the minimum of a list
minimum_of_list([X], X).
minimum_of_list(List,Min):-
    List = [H|T],
    minimum_of_list(T,Min1),
    Min is min(H,Min1).

%*************************************************
% 10 points
% find_nearest_type(+State, +ObjectType, -ObjKey, -Object, -Distance) :- .
%*************************************************

%to calculate the nearest object of a certain type I used many helper predicates
%my aim is to first find the minimum distance between an object of given type and the agent
%then while iterating over objects if I encouter the minimum distance I will return the object

%converts the object dictionary to lists of pairs, values, keys respectively
convert_object_dict_to_list(ObjectDict,ObjectList,ObjectPairs,ObjectValues,ObjectKeys):-
    dict_pairs(ObjectDict,_,ObjectList),
    pairs_keys_values(ObjectList,ObjectKeys,ObjectValues).


%finds the lists of objects of a given type, does not include other types
find_list_of_type(State,ObjectType,ObjectsOfType):-
    State=[AgentDict,ObjectDict,Time],
    convert_object_dict_to_list(ObjectDict,ObjectList,ObjectPairs,ObjectValues,ObjectKeys),
    findall(TempObject,(member(TempObject,ObjectValues),TempObject.type=ObjectType),ObjectsOfType).

%find the list of distances between the agent and the objects of a given type, Distances variable is the list of distances
find_distances_of_type(State,ObjectType,ObjectsOfType,Distances):-
    State=[AgentDict,ObjectsDict,Time],
    find_list_of_type(State,ObjectType,ObjectsOfType),
    findall(Distance,(member(TempObject,ObjectsOfType),manhattan_distance([AgentDict.x,AgentDict.y],[TempObject.x,TempObject.y],Distance)),Distances).    

%finds the minimum distance between the agent and the objects of a given type, returns the minimum distance
find_nearest_type_helper(State,ObjectType,MinDist):-
    find_list_of_type(State,ObjectType,ObjectsOfType),
    find_distances_of_type(State,ObjectType,ObjectsOfType,Distances),
    minimum_of_list(Distances,MinDist).

%finds the nearest object and its properties of a given type
find_nearest_type(State,ObjectType,ObjKey,Object,Distance):-
    State=[AgentDict,ObjectsDict,Time],
    find_nearest_type_helper(State,ObjectType,MinDist),
    dict_pairs(ObjectsDict,_,ObjectList),
    pairs_keys_values(ObjectList,ObjectKeys,ObjectValues),
    member(Key-Value,ObjectList),
    Value.type=ObjectType,
    manhattan_distance([AgentDict.x,AgentDict.y],[Value.x,Value.y],TempDist),
    TempDist==MinDist,
    ObjKey=Key,
    Object=Value,
    Distance=TempDist.

%*************************************************
% 10 points
% navigate_to(+State, +X, +Y, -ActionList, +DepthLimit) :- .
%*************************************************

%checks if the agent has arrived the destination
check_arrived(AgentDict,X,Y):-
    AgentDict.x=Ax,
    AgentDict.y=Ay,
    Ax==X,Ay==Y.


%agent goes to the destination within the range of depth limit, action is the list of actions to get there
navigate_to(State,X,Y,ActionList,DepthLimit):-

    State=[AgentDict,ObjectsDict,Time],
    get_dict(x,AgentDict,Ax),
    get_dict(y,AgentDict,Ay),
    DepthLimit>=0,
    (
        check_arrived(AgentDict,X,Y)->!,ActionList=[];%if arivered, return empty list,else compare position and destination and decide the next action
        (
            Ax<X -> TempList=[go_right],
            NewAx is Ax+1,%go right
            put_dict(x,AgentDict,NewAx,TempAgentDict),
            TempState = [TempAgentDict,ObjectsDict,Time],    
            DepthLimit1 is DepthLimit-1,
        
            navigate_to(TempState,X,Y,ActionList1,DepthLimit1),%after moved (ie.right), call the predicate again to calculate the next actions 
            append(TempList,ActionList1,ActionList)

        );
        (
            Ax>X -> TempList=[go_left],
            NewAx is Ax-1,%go left
            put_dict(x,AgentDict,NewAx,TempAgentDict),
            TempState = [TempAgentDict,ObjectsDict,Time],
            DepthLimit1 is DepthLimit-1,
            
            navigate_to(TempState,X,Y,ActionList1,DepthLimit1),%after moved (ie.left), call the predicate again to calculate the next actions
            append(TempList,ActionList1,ActionList)
        );
        (
            Ay<Y -> TempList=[go_down],
            NewAy is Ay+1,%go down
            put_dict(y,AgentDict,NewAy,TempAgentDict),
            TempState = [TempAgentDict,ObjectsDict,Time],
            DepthLimit1 is DepthLimit-1,
            
            navigate_to(TempState,X,Y,ActionList1,DepthLimit1),%after moved (ie.down), call the predicate again to calculate the next actions
            append(TempList,ActionList1,ActionList)

        );
        (
            Ay>Y -> TempList=[go_up],
            NewAy is Ay-1,%go up
            put_dict(y,AgentDict,NewAy,TempAgentDict),
            TempState = [TempAgentDict,ObjectsDict,Time],
            DepthLimit1 is DepthLimit-1,

            navigate_to(TempState,X,Y,ActionList1,DepthLimit1),%after moved (ie.up), call the predicate again to calculate the next actions
            append(TempList,ActionList1,ActionList)

        )
    ).
    

%*************************************************
% 10 points
% chop_nearest_tree(+State, -ActionList) :- .
%*************************************************

%controls if agent has stone axe
has_stone_axe(AgentDict):-
    get_dict(inventory,AgentDict,Bag),
    dict_pairs(Bag,_,BagList),
    member(stone_axe-_,BagList).
    
%fills the list of actoins to chop the nearest tree 
chop_nearest_tree(State,ActionList):-
    find_nearest_type(State,tree,ObjKey,Object,Distance),
    get_dict(x,Object,Ox),
    get_dict(y,Object,Oy),
    navigate_to(State,Ox,Oy,NavigationActs,Distance),
    State=[AgentDict,ObjectsDict,Time],
    (
        has_stone_axe(AgentDict)->
        ChopActs=[left_click_c,left_click_c,left_click_c,left_click_c];%I was going to make the number of left_click_c depends on the tool agent has but I learned that it is not necessary
        (
            ChopActs=[left_click_c,left_click_c,left_click_c,left_click_c]
        )
    ),
    append(NavigationActs,ChopActs,ActionList).
%*************************************************
% 10 points
% mine_nearest_stone(+State, -ActionList) :- .
%*************************************************

%controls if agent has stone pickaxe
has_stone_pickaxe(AgentDict):-
    get_dict(inventory,AgentDict,Bag),
    dict_pairs(Bag,_,BagList),
    member(stone_pickaxe-_,BagList).

%fills list of actions to mine the nearest stone
mine_nearest_stone(State,ActionList):-
    find_nearest_type(State,stone,ObjKey,Object,Distance),
    get_dict(x,Object,Ox),
    get_dict(y,Object,Oy),
    navigate_to(State,Ox,Oy,NavigationActs,Distance),
    State=[AgentDict,ObjectsDict,Time],
    (
        has_stone_pickaxe(AgentDict)->
        MineActs=[left_click_c,left_click_c,left_click_c,left_click_c];%I was going to make the number of left_click_c depends on the tool agent has but I learned that it is not necessary
        (
            MineActs=[left_click_c,left_click_c,left_click_c,left_click_c]
        )
    ),
    append(NavigationActs,MineActs,ActionList).

%fills list of actions to mine the nearest cobblestone, it is not the wanted predicate but it is needed
mine_nearest_cobblestone(State,ActionList):-
    find_nearest_type(State,cobblestone,ObjKey,Object,Distance),
    get_dict(x,Object,Ox),
    get_dict(y,Object,Oy),
    navigate_to(State,Ox,Oy,NavigationActs,Distance),
    State=[AgentDict,ObjectsDict,Time],
    (
        has_stone_pickaxe(AgentDict)->
        MineActs=[left_click_c,left_click_c,left_click_c,left_click_c];%I was going to make the number of left_click_c depends on the tool agent has but I learned that it is not necessary
        (
            MineActs=[left_click_c,left_click_c,left_click_c,left_click_c]
        )
    ),
    append(NavigationActs,MineActs,ActionList).
%*************************************************
% 10 points
% gather_nearest_food(+State, -ActionList) :- .
%*************************************************

%fills list of actions to gather the nearest food
gather_nearest_food(State,ActionList):-
    find_nearest_type(State,food,ObjKey,Object,Distance),
    get_dict(x,Object,Ox),
    get_dict(y,Object,Oy),
    navigate_to(State,Ox,Oy,NavigationActs,Distance),
    GatherActs=[left_click_c],
    append(NavigationActs,GatherActs,ActionList).

%*************************************************
% 10 points
% collect_requirements(+State, +ItemType, -ActionList) :- .
%*************************************************

%looks agent dictionary to find out how many logs he has
how_many_logs(State,Number):-
    State=[AgentDict,ObjectsDict,Time],
    get_dict(inventory,AgentDict,Bag),
    dict_pairs(Bag,_,BagList),
    member(log-Number,BagList);
    Number=0.
%looks agent dictionary to find out how many sticks he has
how_many_sticks(State,Number):-
    State=[AgentDict,ObjectsDict,Time],
    get_dict(inventory,AgentDict,Bag),
    dict_pairs(Bag,_,BagList),
    member(stick-Number,BagList);
    Number=0.
%looks agent dictionary to find out how many cooblestones he has
how_many_cobblestone(State,Number):-
    State=[AgentDict,ObjectsDict,Time],
    get_dict(inventory,AgentDict,Bag),
    dict_pairs(Bag,_,BagList),
    member(cobblestone-Number,BagList);
    Number=0.

%fills list of actions to collect the requirements of stick 
collect_requirements_stick(State,ActionList):-
    how_many_logs(State,LogNumber),        
    (
        LogNumber>=2->ActionList=[];%if he has more than two logs, he will not need to do any actions
        (
            chop_nearest_tree(State,ActionList)
        )
    ).

%fills the list of actions to collect given number of cobblestone
collect_cobblestone_number(State,0,[]).
collect_cobblestone_number(State,Number,ActionList):-
    Left is Number-1,
    mine_nearest_cobblestone(State,ActionList1),!,
    execute_actions(State,ActionList1,State1),
    collect_cobblestone_number(State1,Left,ActionList2),
    append(ActionList1,ActionList2,ActionList).


%fills the list of actions to collect required items for stone_pickaxe
collect_requirements_stone_pickaxe(State,ActionList):-
   
   %check number of sticks, if it is less than two, he will need to collect more sticks
    how_many_sticks(State,StickNumber),
   (
        StickNumber>=2->ActsForStick=[];
        (
            collect_requirements_stick(State,ActsForStick1),
            append(ActsForStick1,[craft_stick],ActsForStick),
            execute_actions(State,ActsForStick,StateAfterStick)
        )
    ),

    %check number of cobblestones, if it is less than tree, he will need to collect more cobblestones    
    how_many_cobblestone(State,CobblestoneNumber),
    (
        CobblestoneNumber>=3->ActsForCobblestone=[];
           
            (mine_nearest_stone(StateAfterStick,ActsForCobblestone),
                execute_actions(StateAfterStick,ActsForCobblestone,StateAfterCobblestone));
            
            (   
                (CobblestoneNumber=0->collect_cobblestone_number(StateAfterStick,3,ActsForCobblestone),
                    execute_actions(StateAfterStick,ActsForCobblestone,StateAfterCobblestone)
                );
                (CobblestoneNumber=1->collect_cobblestone_number(StateAfterStick,2,ActsForCobblestone),
                    execute_actions(StateAfterStick,ActsForCobblestone,StateAfterCobblestone)
                );
                
                (CobblestoneNumber=2->collect_cobblestone_number(StateAfterStick,1,ActsForCobblestone),
                    execute_actions(StateAfterStick,ActsForCobblestone,StateAfterCobblestone)
                ) 
                
            )

    ),
    
    %check number of logs, if it is less than two, he will need to collect more logs
    how_many_logs(State,LogNumber),
    (
        LogNumber>=3->ActsForLog=[];
        (
            chop_nearest_tree(StateAfterCobblestone,ActsForLog),
            execute_actions(StateAfterCobblestone,ActsForLog,_)
            
        )
    ),

    append(ActsForStick,ActsForCobblestone,TempList),
    append(TempList,ActsForLog,ActionList).
    
%fills the list of actions to collect required items for stone_axe, it is implemented as same with stone_pickaxe (considered the case needed items to craft stone_axe might change)
collect_requirements_stone_axe(State,ActionList):-
    how_many_sticks(State,StickNumber),
    
   (
        StickNumber>=2->ActsForStick=[];
        (
            collect_requirements_stick(State,ActsForStick1),
            append(ActsForStick1,[craft_stick],ActsForStick),
            execute_actions(State,ActsForStick,StateAfterStick)
        )
    ),
    
    
    how_many_cobblestone(State,CobblestoneNumber),
    (
        CobblestoneNumber>=3->ActsForCobblestone=[];
           
            (mine_nearest_stone(StateAfterStick,ActsForCobblestone),
                execute_actions(StateAfterStick,ActsForCobblestone,StateAfterCobblestone));
            
            (   
                (CobblestoneNumber=0->collect_cobblestone_number(StateAfterStick,3,ActsForCobblestone),
                    execute_actions(StateAfterStick,ActsForCobblestone,StateAfterCobblestone)
                );
                (CobblestoneNumber=1->collect_cobblestone_number(StateAfterStick,2,ActsForCobblestone),
                    execute_actions(StateAfterStick,ActsForCobblestone,StateAfterCobblestone)
                );
                
                (CobblestoneNumber=2->collect_cobblestone_number(StateAfterStick,1,ActsForCobblestone),
                    execute_actions(StateAfterStick,ActsForCobblestone,StateAfterCobblestone)
                ) 
                
            )

    ),
    
    
    how_many_logs(State,LogNumber),
    (
        LogNumber>=3->ActsForLog=[];
        (
            chop_nearest_tree(StateAfterCobblestone,ActsForLog),
            execute_actions(StateAfterCobblestone,ActsForLog,_)
            
        )
    ),

    append(ActsForStick,ActsForCobblestone,TempList),
    append(TempList,ActsForLog,ActionList).
%using collect_requirements_stone_pickaxe, collect_requirements_stone_axe,collect_requirements_stick to collect required items
collect_requirements(State,ItemType,ActionList):-
    
    (ItemType=stone_pickaxe->collect_requirements_stone_pickaxe(State,ActionList));
    (ItemType=stone_axe->collect_requirements_stone_axe(State,ActionList));
    (ItemType=stick->collect_requirements_stick(State,ActionList)).

%************************************************* 
% 5 points
% find_castle_location(+State, -XMin, -YMin, -XMax, -YMax) :- .
%*************************************************
%helper predicate to check if a tile is occupied
tile_occupied_for_castle(ObjectDict,X,Y):-
    get_dict(_,ObjectDict,Obj),
    get_dict(x,Obj,Ox),
    get_dict(y,Obj,Oy),
    X=Ox,Y=Oy.

%I wanted to create a list of all possible tiles to check, in order to find the castle location
%beetween the edges fill an array with numbers->start=2 end=5, list=[2,3,4,5]
create_list_of_possibilities(Start,Start,List):-
    List=[Start].
create_list_of_possibilities(Start,End,List):-
    Start=<End,
    NewStart is Start+1,
    create_list_of_possibilities(NewStart,End,Rest),
    List = [Start|Rest].

%this is helper predicate to add a number to a list to get a list of 2 length lists-> Element=2 List[1,3] , result=[[2,1][2,3]]
combination_of_element_list(Element,[X],[[Element,X]]).
combination_of_element_list(Element,List,Result):-
    List=[X|Rest],
    combination_of_element_list(Element,Rest,Result1),
    Result=[[Element,X]|Result1].

%this is another helper predicate put all cordinates to a list
combinations_of_lists([],_,[]).
combinations_of_lists(List1,List2,List):-
    [H1|T1]=List1,
    [H2|T2]=List2,
    combination_of_element_list(H1,List2,TempList),
    combinations_of_lists(T1,List2,TempList2),
    append(TempList,TempList2,List).


%this predicate uses the helper ones and applys them with borders of map to get all possible tiles to check
find_positions_to_check(State,List):-
    State=[AgentDict,ObjectsDict,Time],
    width(W),_width is W-3,
    height(H),_height is H-3,
    \+ W<5,
    \+ H<5,
    create_list_of_possibilities(2,_width,Rows),
    create_list_of_possibilities(2,_height,Columns),
    combinations_of_lists(Rows,Columns,List).


%this predicate checks if 9 tiles(all the neighbourhood and center) are occupied or not 
check_neighbour(State,X,Y):-
    State=[AgentDict,ObjectsDict,Time],
    LeftX is X-1,
    RightX is X+1,
    UpY is Y-1,
    DownY is Y+1,
    \+tile_occupied_for_castle(ObjectsDict,LeftX,UpY),
    \+tile_occupied_for_castle(ObjectsDict,X,UpY),
    \+tile_occupied_for_castle(ObjectsDict,RightX,UpY),
    \+tile_occupied_for_castle(ObjectsDict,LeftX,Y),
    \+tile_occupied_for_castle(ObjectsDict,X,Y),
    \+tile_occupied_for_castle(ObjectsDict,RightX,Y),
    \+tile_occupied_for_castle(ObjectsDict,LeftX,DownY),
    \+tile_occupied_for_castle(ObjectsDict,X,DownY),
    \+tile_occupied_for_castle(ObjectsDict,RightX,DownY).

%traverse over all the possible tiles to check and find the castle location by checking if neighbours are occupied
find_castle_location(State,XMin,YMin,XMax,YMax):-
    State=[AgentDict,ObjectsDict,Time],
    find_positions_to_check(State,List),
    member([X,Y],List),
    check_neighbour(State,X,Y),
    XMin is X-1,
    YMin is Y-1,
    XMax is X+1,
    YMax is Y+1.


%*************************************************
% 15 points
% make_castle(+State, -ActionList) :- .
%*************************************************
%while implenting this predicate my intention was mine stones until agents get 9 cobblestones or no stones left, then mine needed cobblestones
%after collecting 9 cobblestones check location and place cobblestones

%checking is there any stone exist on map
check_stone_exist(State):-
    State=[AgentDict,ObjectsDict,Time],
    find_nearest_type(State,stone,Key,Obj,Dist).   


%predicate to determine stone should be mined or not
no_need_stone(State):-
    State=[AgentDict,ObjectsDict,Time],
    how_many_cobblestone(State,CobblestoneNumber),
    CobblestoneNumber>=9->(true);
    \+check_stone_exist(State)->(true).

mine_stone_until_castle(State,[]):-
    State=[AgentDict,ObjectsDict,Time],
    \+no_need_stone(State),

%predicate to mine stone until it is not needed or no stone left
mine_stone_until_castle(State,ActionList):-
    State=[AgentDict,ObjectsDict,Time],
    how_many_cobblestone(State,CobblestoneNumber),
    (
        \+no_need_stone(State)->
        (
            mine_nearest_stone(State,ActionList1),
            execute_actions(State,ActionList1,TempState),
            mine_stone_until_castle(TempState,ActionList2)
        );(ActionList1=[],ActionList2=[])
    ),
    
    append(ActionList1,ActionList2,ActionList).

%collects 9 cobblestones priotizes stones over cobblestones
collect_nine_cobblestone(State,ActionList):-
    State=[AgentDict,ObjectsDict,Time],
    how_many_cobblestone(State,CobblestoneNumber),
    (
        CobblestoneNumber>=9->ActionList1=[];
        (   
            mine_stone_until_castle(State,ActionList1)
        )
    ),

    execute_actions(State,ActionList1,StateAfterMine1),
    how_many_cobblestone(StateAfterMine1,CobblestoneNumberAfter),
    Target is 9-CobblestoneNumberAfter, 
    collect_cobblestone_number(StateAfterMine1,Target,ActionList2),
    append(ActionList1,ActionList2,ActionList).

%collects 9 cobblestones, find location after collecting and place cobblestones
make_castle(State,ActionList):-
    State=[AgentDict,ObjectsDict,Time],
    collect_nine_cobblestone(State,ActionList1),!,
    execute_actions(State,ActionList1,StateAfterCollect),
    StateAfterCollect=[AgentDictAfter,ObjectsDictAfter,TimeAfter],
    get_dict(x,AgentDictAfter,CurrX),
    get_dict(y,AgentDictAfter,CurrY),
    find_castle_location(StateAfterCollect,XMin,YMin,XMax,YMax),
    X is XMin+1,
    Y is YMin+1,
    manhattan_distance([CurrX,CurrY],[X,Y],Dist),
    navigate_to(StateAfterCollect,X,Y,ActionList2,Dist),
    ActionList3=[place_c,place_e,place_n,place_w,place_s,place_ne,place_nw,place_sw,place_se],
    append(ActionList1,ActionList2,Temp),
    append(Temp,ActionList3,ActionList).





