-module(ex5_214237638).
-export([ring_parallel/2, ring_serial/2,mesh_parallel/3,mesh_serial/3]).
-define(MICRO_TO_MILLI, 1000).

%% -------------------- ring_parallel(N,M) -------------------------------------------- %%
% Spread M copies of a message from the head process toward a ring of N processes
ring_parallel(N, M) ->
    StartTime = erlang:timestamp(),
    
    % Initialize process dictionary for shell process
    put(startTime, StartTime), 
    put(dupsAmount, M),
    put(sent_count, 0),
    put(received_count, 0),
    put(status, onRun),
    
    % Create the ring with shell as first process
    Containing_List = [self()],
    List = ringProcessLoop(N-1, M, Containing_List, self()),
    
    % Form the ring
    RingData = List ++ [self()],
    io:format("~n Ring created: ~w ~n", [RingData]),
    
    % Store ring for later use
    put(ring, RingData),
    
    % Handle the START message in the shell process
    self() ! {start, RingData, M},
    
    % Enter message processing loop
    shell_process_loop().

% Message processing loop for the shell process
shell_process_loop() ->
    receive
        {start, RingData, M} ->
            io:format("Shell process ~p received START message with M=~p~n", [self(), M]),
            % Get the next process in the ring
            [NextProcess|RestOfRing] = tl(RingData),
            io:format("Shell process ~p is sending ~p messages to next process ~p~n", [self(), M, NextProcess]),
            
            % Generate and send M messages
            Messages = lists:seq(1, M),
            lists:map(fun(Message_Index)-> 
                io:format("Shell process ~p sending message #~p to ~p~n", [self(), Message_Index, NextProcess]),
                put(sent_count, get(sent_count) + 1),
                (NextProcess ! {RestOfRing, Message_Index, self()})
            end, Messages),
            shell_process_loop();
            
        {[], MessageNumber, From} ->
            put(received_count, get(received_count) + 1),
            io:format("Shell process ~p received COMPLETED message #~p from ~p~n", [self(), MessageNumber, From]),
            M = get(dupsAmount),
            if
                MessageNumber == M ->
                    EndTime = erlang:timestamp(),
                    Sent = get(sent_count),
                    Received = get(received_count),
                    io:format("Shell process ~p received final message #~p with sent=~p, received=~p~n", 
                              [self(), MessageNumber, Sent, Received]),
                    ElapsedTime = timer:now_diff(EndTime, get(startTime))/?MICRO_TO_MILLI,
                    io:format("~n[All messages completed the ring! Total time: ~pms]~n", [ElapsedTime]),
                    put(status, done),
                    put(return_message, {ElapsedTime, Sent, Received});
                true -> 
                    io:format("Shell process ~p received message #~p (not final)~n", [self(), MessageNumber]),
                    shell_process_loop()
            end;
            
        {[H|T], MessageNumber, From} ->
            put(received_count, get(received_count) + 1),
            io:format("Shell process ~p received message #~p from ~p, forwarding to ~p~n", [self(), MessageNumber, From, H]),
            put(sent_count, get(sent_count) + 1),
            H ! {T, MessageNumber, self()},
            shell_process_loop();
            
        {H, MessageNumber, From} ->
            put(received_count, get(received_count) + 1),
            io:format("Shell process ~p received message #~p with single element from ~p, forwarding to ~p~n", [self(), MessageNumber, From, H]),
            put(sent_count, get(sent_count) + 1),
            H ! {[], MessageNumber, self()},
            shell_process_loop();
            
        _anyMessage->
            io:format("~n Shell process ~p received unknown message~n", [self()]),
            shell_process_loop()
    end,
    
    % Check if we're done and should return
    Status = get(status),
    if
        Status == done ->
            % Get the stored ring directly
            CurrentRing = get(ring),
            % Filter out the shell process (self())
            SpawnedProcesses = lists:filter(fun(Pid) -> Pid =/= self() end, CurrentRing),
            % Terminate all spawned processes
            lists:map(fun(Pid) -> exit(Pid, finished) end, SpawnedProcesses),
            get(return_message);
        true->
            shell_process_loop() 
    end.

% ------ ringProcessLoop --------------
ringProcessLoop(0, _M, List, _Pid_of_Calling_Function) ->
    List;

ringProcessLoop(Number, M, List, _Pid_of_Calling_Function) ->
    % Get the last process to be the caller for the new process
    LastProcess = case List of
        [] -> self();  % If list is empty, use shell process
        _ -> lists:last(List)  % Otherwise use the last process in the list
    end,
    
    Pid_Of_New_Node = spawn(fun() -> nodeProcess(LastProcess, M) end),
    ringProcessLoop(Number - 1, M, List ++ [Pid_Of_New_Node], Pid_Of_New_Node).

% ------ nodeProcess ---------------
nodeProcess(Pid, M) ->                                                                     
    put(pid_of_caller, Pid), put(dups_Amount, M), put(status, onRun),
    % Initialize message counters for this process
    put(sent_count, 0),
    put(received_count, 0),
    io:format("Process ~p initialized with caller ~p and M=~p~n", [self(), Pid, M]),
    nodeProcess().

nodeProcess()->
    receive
        {start, Ring, M}->
            io:format("Process ~p received START message with M=~p~n", [self(), M]),
            Messages = lists:seq(1, M),
            [_Self|[NextProcess|RestOfRing]] = Ring,  % Skip yourself, take the next process
            io:format("Process ~p is sending ~p messages to next process ~p~n", [self(), M, NextProcess]),
            lists:map(fun(Message_Index)-> 
                io:format("Process ~p sending message #~p to ~p~n", [self(), Message_Index, NextProcess]),
                put(sent_count, get(sent_count) + 1),  % Increment sent counter
                (NextProcess!{RestOfRing, Message_Index, self()}) 
            end, Messages);
        
        {[], MessageNumber, From} ->
            put(received_count, get(received_count) + 1),  % Increment received counter
            io:format("Process ~p received COMPLETED message #~p from ~p~n", [self(), MessageNumber, From]),
            M = get(dups_Amount),
            if
                MessageNumber == M ->
                    God = get(pid_of_caller),
                    EndTime = erlang:timestamp(),
                    Sent = get(sent_count),
                    Received = get(received_count),
                    io:format("Process ~p received final message #~p, sending DONE to ~p with sent=~p, received=~p~n", 
                              [self(), MessageNumber, God, Sent, Received]),
                    put(status, done),
                    God ! {done, EndTime, Sent, Received};  % Include message counts
                true -> 
                    io:format("Process ~p received message #~p (not final)~n", [self(), MessageNumber])
            end;                    
                
        {[H|T], MessageNumber, From} ->
            put(received_count, get(received_count) + 1),  % Increment received counter
            io:format("Process ~p received message #~p from ~p, forwarding to ~p~n", [self(), MessageNumber, From, H]),
            put(sent_count, get(sent_count) + 1),  % Increment sent counter
            H ! {T, MessageNumber, self()};  

        {H, MessageNumber, From} ->
            put(received_count, get(received_count) + 1),  % Increment received counter
            io:format("Process ~p received message #~p with single element from ~p, forwarding to ~p~n", [self(), MessageNumber, From, H]),
            put(sent_count, get(sent_count) + 1),  % Increment sent counter
            H ! {[], MessageNumber, self()};      
                     
        _anyMessage ->
            io:format("Process ~p received UNKNOWN message~n", [self()]),
            unknown_message_received            
    end,
    Status = get(status),
    if
        Status /= done ->
            nodeProcess();
        true-> 
            io:format("Process ~p is DONE and exiting~n", [self()]),
            ok
    end.

    
%% -------------------- ring_seriall(N,M)  ------------------------------------------------------------------------------------------------------------ %%
% Spread M copies of a message from the head process toward a ring of N nodes (in a serial implementation)
%% -------------------- ring_serial(V,M) -------------------------------------------- %%
% Spread M copies of a message through a virtual ring of V vertices using a single process
ring_serial(V, M) ->
    Start_Time = erlang:timestamp(),                                                       % Measure the starting time of the function
    put(start_time, Start_Time),
    put(dups_amount, M),
    put(sent_count, 0),
    put(received_count, 0),
    
    % Create a virtual ring of vertices (1 to V with 1 repeated at the end)
    List = lists:seq(1, V), 
    [H | _T] = List,
    Ring = List ++ [H],
    io:format("~n here is the ring:~p ~n", [Ring]),
    
    % Create M messages and simulate sending them from vertex 1
    Messages = lists:seq(1, M),
    lists:foreach(fun(MessageIndex) -> 
        io:format("Starting message #~p in the ring~n", [MessageIndex]),
        put(sent_count, get(sent_count) + 1),
        self() ! {Ring, MessageIndex}
    end, Messages),
    
    % Process messages until done
    process_ring_messages().

process_ring_messages() ->
    receive
        % Message has completed the ring (empty path)
        {[], MessageIndex} ->
            put(received_count, get(received_count) + 1),
            io:format("Message #~p completed the ring~n", [MessageIndex]),
            M = get(dups_amount),
            if          
                MessageIndex == M ->
                    io:format("Final message #~p received, terminating~n", [MessageIndex]),
                    self() ! {done};
                true -> 
                    ok
            end,
            process_ring_messages();
            
        % Message is moving through the ring
        {[H|T], MessageIndex} ->
            io:format("Message #~p moving to vertex ~p~n", [MessageIndex, H]),
            put(sent_count, get(sent_count) + 1),
            self() ! {T, MessageIndex},
            process_ring_messages();

        % Last vertex in ring sends to first vertex
        {H, MessageIndex} ->
            io:format("Message #~p at last vertex, sending to vertex ~p~n", [MessageIndex, H]),
            put(sent_count, get(sent_count) + 1),
            self() ! {[], MessageIndex},
            process_ring_messages();

        % Processing complete
        {done} ->
            EndTime = erlang:timestamp(),
            StartTime = get(start_time),
            Sent = get(sent_count),
            Received = get(received_count),
            ElapsedTime = timer:now_diff(EndTime, StartTime)/?MICRO_TO_MILLI,
            io:format("~n[All messages processed! Time: ~pms, Sent: ~p, Received: ~p]~n", 
                      [ElapsedTime, Sent, Received]),
            {ElapsedTime, Sent, Received};
            
        % Handle unexpected messages
        _Other ->
            io:format("Received unexpected message~n"),
            process_ring_messages()
    end.

%% -------------------- mesh_parallel(N,M, C)  ------------------------------------------------------------------------------------------------------------ %%
% Spread M copies of a message from a main process toward a mesh of NxN processes (avoid duplications/ messages storms)


validate_leader(N, C) ->
    if 
        C < 1 orelse C > N*N -> 
            io:format("Error: Leader value C=~p is invalid! Valid range: 1 to ~p~n", [C, N*N]),
            exit({error, invalid_leader_value});
        true -> 
            C
    end.

mesh_parallel(N, M, C) ->
    % Start timing
    StartTime = erlang:system_time(microsecond),
    
    % Store the caller for final notification
    Parent = self(),
    
    % Validate leader
    ValidC = validate_leader(N, C),
    
    % Create registry process
    Registry = spawn(fun() -> 
        registry_loop(#{}, 0, N*N, Parent, ValidC, M, StartTime)
    end),
    
    % Create all processes
    [spawn_process(Index, N, Registry, ValidC) || Index <- lists:seq(1, N*N)],
    
    % Wait for completion message from registry
    receive
        {mesh_communication_complete, TotalTime, Sent, Received} ->
            {TotalTime, Sent, Received}
    end.

registry_loop(Processes, ReadyCount, TotalProcesses, Parent, LeaderIndex, M, StartTime) ->
    
    receive
        {register, Index, Pid} ->
            ProcessCount = maps:size(Processes) + 1,
            UpdatedProcesses = maps:put(Index, Pid, Processes),
            
            % If all processes registered, tell them to calculate neighbors
            case ProcessCount == TotalProcesses of
                true ->
                    maps:map(
                        fun(_I, ProcPid) ->
                            ProcPid ! {start_neighbor_calc, self()}
                        end,
                        UpdatedProcesses
                    );
                false ->
                    ok
            end,
            registry_loop(UpdatedProcesses, ReadyCount, TotalProcesses, Parent, LeaderIndex, M, StartTime);
        
        {process_ready, Index} ->
            % One more process is ready
            NewReadyCount = ReadyCount + 1,
            
            % If all processes ready, tell leader to start communication
            if 
                NewReadyCount =:= TotalProcesses ->
                 
                    
                    case maps:find(LeaderIndex, Processes) of
                        {ok, LeaderPid} ->
                            LeaderPid ! {start_generating_messages, M, self()};
                        error ->
                            io:format("ERROR: Leader process ~p not found in registry!~n", [LeaderIndex]),
                            EndTime = erlang:system_time(microsecond),
                            TotalTime = (EndTime - StartTime) div ?MICRO_TO_MILLI,
                            Parent ! {mesh_communication_complete, TotalTime, 0, 0}
                    end;
                true ->
                    % Still waiting for more processes
                    ok
            end,
            registry_loop(Processes, NewReadyCount, TotalProcesses, Parent, LeaderIndex, M, StartTime);
        
        % Communication complete message with message counts
        {communication_complete, Sent, Received} ->
            EndTime = erlang:system_time(microsecond),
            TotalTime = (EndTime - StartTime) div ?MICRO_TO_MILLI,
            Parent ! {mesh_communication_complete, TotalTime, Sent, Received},
            registry_loop(Processes, ReadyCount, TotalProcesses, Parent, LeaderIndex, M, StartTime);
        
        % Process lookup
        {lookup, Index, ReplyTo} ->
            Reply = case maps:find(Index, Processes) of
                {ok, Pid} -> {pid_of, Index, Pid};
                error -> {pid_not_found, Index}
            end,
            ReplyTo ! Reply,
            registry_loop(Processes, ReadyCount, TotalProcesses, Parent, LeaderIndex, M, StartTime);
        
        {get_all, ReplyTo} ->
            ReplyTo ! {all_processes, Processes},
            registry_loop(Processes, ReadyCount, TotalProcesses, Parent, LeaderIndex, M, StartTime);
            
        stop ->
            maps:foreach(fun(_Index, Pid) -> Pid ! stop end, Processes)
    end.

% Spawn a process
spawn_process(Index, N, Registry, LeaderIndex) ->
    IsLeader = (Index =:= LeaderIndex),
    LeaderText = if IsLeader -> " (LEADER)"; true -> "" end,
    
    Pid = spawn(fun() -> 
        process_function(Index, N, LeaderIndex) 
    end),
    
    Pid ! {get_registry, Registry},
    Registry ! {register, Index, Pid},
    Pid.

% Process initialization
process_function(Index, N, LeaderIndex) ->
    IsLeader = (Index =:= LeaderIndex),
    LeaderText = if IsLeader -> " (LEADER)"; true -> "" end,
    
    receive
        {get_registry, Registry} ->

            receive
                {start_neighbor_calc, Registry} ->
                    % Calculate neighbors

                    Neighbors = get_neighbors(Index, N),

                    
                    % Notify registry ready
                    Registry ! {process_ready, Index},
                    
                    % Initialize empty sets for tracking seen messages and responses
                    SeenMessages = sets:new(),
                    RespondedMessages = sets:new(),
                    
                    % Initialize counters
                    SentCount = 0,
                    ReceivedCount = 0,
                    

                    process_loop(Index, N, Neighbors, Registry, IsLeader, LeaderIndex, 
                                SeenMessages, RespondedMessages, SentCount, ReceivedCount);
                
                Other ->
                    process_function(Index, N, LeaderIndex)
            end;

        Other ->
            process_function(Index, N, LeaderIndex)
    end.

% Main process loop
process_loop(Index, N, Neighbors, Registry, IsLeader, LeaderIndex, SeenMessages, RespondedMessages, SentCount, ReceivedCount) ->
    LeaderText = if IsLeader -> " (LEADER)"; true -> "" end,
    
    receive
        % Leader starts generating messages
        {start_generating_messages, M, _ReplyTo} when IsLeader ->

            
            % Generate and send M messages to neighbors
            {NewSentCount, NewSeenMessages} = generate_messages(Index, M, Neighbors, Registry, SentCount, SeenMessages),

            % Wait for all responses (M from each node except self)
            ExpectedResponses = M * (N*N - 1),
            leader_collect_responses(Index, ExpectedResponses, Registry, #{}, ReceivedCount, NewSentCount, 
                                   N, Neighbors, LeaderIndex, NewSeenMessages, RespondedMessages);
        
        % Process receives a message
        {message, FromIndex, {SourceIndex} = Message, ReplyTo} ->
            % Update received counter
            NewReceivedCount = ReceivedCount + 1,
            
            % Check if message already seen
            MessageKey = {SourceIndex},
            case sets:is_element(MessageKey, SeenMessages) of
                true ->
                    % Already seen this message, ignore it
                    ReplyTo ! {message_duplicate, Index},
                    process_loop(Index, N, Neighbors, Registry, IsLeader, LeaderIndex, 
                                SeenMessages, RespondedMessages, SentCount, NewReceivedCount);
                    
                false ->
                    % New message, mark as seen
                    UpdatedSeenMessages = sets:add_element(MessageKey, SeenMessages),
                    
                    % Check if we should respond (once per message)
                    {UpdatedRespondedMessages, NewSentCount} = 
                        if not IsLeader ->
                            % Check if already responded to this message
                            case sets:is_element(MessageKey, RespondedMessages) of
                                true ->
                                    % Already responded,
                                    {RespondedMessages, SentCount};
                                false ->
                                    % Send response to leader
                                    Registry ! {lookup, LeaderIndex, self()},
                                    receive
                                        {pid_of, LeaderIndex, LeaderPid} ->
                                            Response = {Index},
                                            LeaderPid ! {response, Response, self()},
                                            {sets:add_element(MessageKey, RespondedMessages), SentCount + 1};
                                        {pid_not_found, _} ->

                                            {RespondedMessages, SentCount}
                                    end
                            end;
                        true ->
                            % Leader doesn't respond
                            {RespondedMessages, SentCount}
                        end,
                    
                    % Forward to other neighbors if:
                    % - not leader (all nodes forward), or
                    % - leader forwarding its own message
                    NewSentCount2 = 
                        if
                            IsLeader andalso SourceIndex =/= Index ->
                                % Leader doesn't forward messages it didn't generate
                                NewSentCount;
                            true ->
                                % Forward to all neighbors except sender
                                OtherNeighbors = lists:delete(FromIndex, Neighbors),
                                lists:foldl(
                                    fun(NeighborIndex, Count) ->
                                        Registry ! {lookup, NeighborIndex, self()},
                                        receive
                                            {pid_of, NeighborIndex, NeighborPid} ->

                                                NeighborPid ! {message, Index, Message, self()},
                                                Count + 1;
                                            {pid_not_found, _} ->
                                                Count
                                        end
                                    end,
                                    NewSentCount,
                                    OtherNeighbors
                                )
                        end,
                    
                    % Confirm receipt
                    ReplyTo ! {message_received, Index},
                    
                    process_loop(Index, N, Neighbors, Registry, IsLeader, LeaderIndex, UpdatedSeenMessages, 
                                UpdatedRespondedMessages, NewSentCount2, NewReceivedCount)
            end;
            
        % Leader receives response from a node
        {response, {ResponseIndex}, ReplyTo} when IsLeader ->
            
            % Acknowledge receipt
            ReplyTo ! {response_received, Index},
            
            % Continue with updated receive counter
            process_loop(Index, N, Neighbors, Registry, IsLeader, LeaderIndex, 
                        SeenMessages, RespondedMessages, SentCount, ReceivedCount + 1);
        
        % Handle message receipt confirmations
        {message_received, FromIndex} ->
            process_loop(Index, N, Neighbors, Registry, IsLeader, LeaderIndex, 
                        SeenMessages, RespondedMessages, SentCount, ReceivedCount);
            
        {message_duplicate, FromIndex} ->
            process_loop(Index, N, Neighbors, Registry, IsLeader, LeaderIndex, 
                        SeenMessages, RespondedMessages, SentCount, ReceivedCount);
            
        {response_received, FromIndex} ->
            process_loop(Index, N, Neighbors, Registry, IsLeader, LeaderIndex, 
                        SeenMessages, RespondedMessages, SentCount, ReceivedCount);
        
        % Handle stop
        stop -> 
            ok;
            
        % Handle unexpected messages
        Msg -> 
            process_loop(Index, N, Neighbors, Registry, IsLeader, LeaderIndex, 
                        SeenMessages, RespondedMessages, SentCount, ReceivedCount)
    end.

% Generate messages and send to neighbors
generate_messages(Index, M, Neighbors, Registry, SentCount, SeenMessages) ->
    
    lists:foldl(
        fun( {Count, SeenMsgs}) ->
            Message = {Index},
            
            % Mark own message as seen
            UpdatedSeenMsgs = sets:add_element(Message, SeenMsgs),
            
            % Send to all neighbors
            NewCount = lists:foldl(
                fun(NeighborIndex, InnerCount) ->
                    Registry ! {lookup, NeighborIndex, self()},
                    receive
                        {pid_of, NeighborIndex, NeighborPid} ->
                            NeighborPid ! {message, Index, Message, self()},
                            InnerCount + 1;
                        {pid_not_found, _} ->
                            InnerCount
                    end
                end,
                Count,
                Neighbors
            ),
            
            {NewCount, UpdatedSeenMsgs}
        end,
        {SentCount, SeenMessages},
        lists:seq(1, M)
    ).

% Leader collects responses
leader_collect_responses(Index, 0, Registry, ResponseMap, ReceivedCount, SentCount, N, Neighbors, LeaderIndex, SeenMessages, RespondedMessages) ->
    % All responses received, notify registry
    TotalUniqueResponses = maps:size(ResponseMap),
    Registry ! {communication_complete, SentCount, ReceivedCount},
    
    % Return to normal loop
    process_loop(Index, N, Neighbors, Registry, true, LeaderIndex, SeenMessages, RespondedMessages, SentCount, ReceivedCount);
    
leader_collect_responses(Index, RemainingResponses, Registry, ResponseMap, ReceivedCount, SentCount, N, Neighbors, LeaderIndex, SeenMessages, RespondedMessages) ->
    
    receive
        {response, {ResponseIndex} = _Response, FromPid} ->
            % Acknowledge receipt
            FromPid ! {response_received, Index},
            
            % Check if duplicate response
            ResponseKey = {ResponseIndex},
            {NewRemainingResponses, NewResponseMap} = 
                case maps:is_key(ResponseKey, ResponseMap) of
                    true ->
                        % Duplicate - don't count towards remaining
                       
                        {RemainingResponses, ResponseMap};
                    false ->
                        % New response - count towards remaining
                        {RemainingResponses - 1, maps:put(ResponseKey, true, ResponseMap)}
                end,
            
            % Continue collecting
            leader_collect_responses(Index, NewRemainingResponses, Registry, NewResponseMap, 
                                   ReceivedCount + 1, SentCount, N, Neighbors, LeaderIndex, 
                                   SeenMessages, RespondedMessages);
            
        % Handle other messages during collection
        {message_received, FromIndex} ->
          
            leader_collect_responses(Index, RemainingResponses, Registry, ResponseMap, 
                                   ReceivedCount, SentCount, N, Neighbors, LeaderIndex, 
                                   SeenMessages, RespondedMessages);
            
        {message_duplicate, FromIndex} ->
         
            leader_collect_responses(Index, RemainingResponses, Registry, ResponseMap, 
                                   ReceivedCount, SentCount, N, Neighbors, LeaderIndex, 
                                   SeenMessages, RespondedMessages);
        
        {message, FromIndex, {SourceIndex}, ReplyTo} ->
            % Ignore messages during collection but count them
          
            ReplyTo ! {message_duplicate, Index},
            leader_collect_responses(Index, RemainingResponses, Registry, ResponseMap, 
                                   ReceivedCount + 1, SentCount, N, Neighbors, LeaderIndex, 
                                   SeenMessages, RespondedMessages)
    end.

% Calculate neighbors for a process at given index
get_neighbors(Index, N) ->
    % Calculate grid position from index (1-based)
    A = (Index-1) div N,
    B = (Index-1) rem N,
    
    
    % Initialize empty neighbor list
    Neighbors = [],
    
    % Add upper neighbor if not at top
    Upper = if A > 0 -> 
                 UpperIndex = (A-1)*N + B + 1,

                 [UpperIndex | Neighbors];
             true -> 
                 Neighbors
           end,
    
    % Add down neighbor if not at bottom
    Down = if A < N-1 -> 
                DownIndex = (A+1)*N + B + 1,
                [DownIndex | Upper];
            true -> 
                Upper
          end,
    
    % Add left neighbor if not at left edge
    Left = if B > 0 -> 
               LeftIndex = A*N + (B-1) + 1,

               [LeftIndex | Down];
           true -> 

               Down
         end,
    
    % Add right neighbor if not at right edge
    Right = if B < N-1 -> 
                RightIndex = A*N + (B+1) + 1,

                [RightIndex | Left];
            true -> 

                Left
          end,
    

    % Return the list of all neighbors
    Right.

%MESH_SERIAL
% 


%% -------------------- mesh_serial(N, M, C)  ------------------------------------------------------------------------------------------------------------ %%
% Spread M copies of a message from a main process toward a mesh of NxN nodes (avoid duplications/ messages storms) - serial implemantation
mesh_serial(N,M,C)->
    StartTime = erlang:timestamp(),                                                 %  Measure the starting time of the function
    Nsquare = N*N,   
    Mesh = lists:seq(1, Nsquare),   
    NeighborsLists = lists:map(fun(Index) -> getNeighbors(N, Index) end, Mesh),     %  Create a neighbors list for each process in the mesh
     
    NeighborsOfMainProcess = lists:nth(C, NeighborsLists),                          %  Compute the neighbors list of the main process
    MailBoxes = [[] || _<- Mesh],                                                   %  Create N^2 empty mailboxes
    put(mesh, Mesh), 
    put(neighbors_lists, NeighborsLists), 
    put(c, C), put(mailboxes, MailBoxes),
    put(m, M), put(start_time, StartTime),  
    lists:map(fun(Dest_Neighbor) -> serialSpreadMessages({C,Dest_Neighbor}, M, M) end, NeighborsOfMainProcess),
    mesh_serial().
 
mesh_serial()-> 
    C = get(c), Nsquare = length(get(mesh)),
    receive   
        {{SourceIndex, MessageNumber}, Destination} ->   % Arguments meaning: (1) Who sent it (2) The nth copy (out of M copies) (3) The destination node                    
            MailBoxesList = get(mailboxes), 
            Dest_MailBox = lists:nth(Destination, MailBoxesList), 
            Already_in_mailBox = lists:member({SourceIndex, MessageNumber}, Dest_MailBox),

            if 
                (not(Already_in_mailBox) and not(SourceIndex == Destination )) -> % No need for 'self acking' so message with same source and destination are ignored
                   
                    Append_MailBox = lists:append(Dest_MailBox, [{SourceIndex, MessageNumber}]),                  
                    if                                   % This 'if' block is used to prevent from trying to access illegal index (smaller that 1 or bigger than N^2)
                        (Destination == 1) ->  
                            Append_MailBoxesList = [Append_MailBox] ++ lists:nthtail(1, MailBoxesList);
                           
                        (Destination == Nsquare) ->                          % It's the N^2 -th node the (last one)     
                            Append_MailBoxesList = lists:sublist(MailBoxesList, length(MailBoxesList) - 1) ++ [Append_MailBox];
                          
                        true ->                                              % Just some node at the middle (index respects:     1 < index < N^2 )
                            Append_MailBoxesList = lists:sublist(MailBoxesList, Destination - 1) ++ [Append_MailBox] ++ lists:nthtail(Destination, MailBoxesList)
                    end,

                    put(mailboxes, Append_MailBoxesList),
                    Receiver_Neighbors_List = lists:nth(Destination, get(neighbors_lists)),                   
                    if
                        (Destination /= C) and (SourceIndex == C)->          %  The receiving node is not the main node and the message is from the main node

                        %   Produce a message with your index and sent it to all of your neighbors and *also* forward the received message:
                            lists:map(fun(Dest_Neighbor) -> self()!{{SourceIndex,MessageNumber},Dest_Neighbor} end, Receiver_Neighbors_List),
                            lists:map(fun(Dest_Neighbor) -> self()!{{Destination,MessageNumber},Dest_Neighbor} end, Receiver_Neighbors_List);

                        ((Destination /= C) and (SourceIndex /= C))->        %  The receiving node is not the main node and the message is not from the main node
                        %   Only forward the receving message:    
                            lists:map(fun(Dest_Neighbor) -> self()!{{SourceIndex,MessageNumber},Dest_Neighbor} end, Receiver_Neighbors_List);

                        true->                                               %  The receiving node is the main node and shall pass no arrival messages
                            DupsAmount = get(m),
                            if
                                length(Append_MailBox) == ((Nsquare - 1) * DupsAmount) ->               
                                    put(status, done);                                   
                                true-> ok     
                            end
                    end;
                true -> 
                    ok
            end;                                                                          
        _anyMessage ->
            unknown_message_received
    end,
    StartTime = get(start_time),
    EndCondition = get(status),  
    if
        EndCondition == done ->
            EndTime = erlang:timestamp(), M = get(m), C = get(c), 
            ElapsedTime = timer:now_diff(EndTime, StartTime)/?MICRO_TO_MILLI,             %  Devide by 1000
            
            {ElapsedTime, M*length(lists:nth(C, get(neighbors_lists))), M*(Nsquare-1)};
        true->
    mesh_serial()
    end.
 % --------------------------------------------------------------    
serialSpreadMessages(_Message, _M_, 0) -> ok;                                             %   Spreads M copies of a message to each element in a given list
serialSpreadMessages({SourceIndex,Dest_Neighbor}, M, _To_Send)->
    self()!{{SourceIndex,M - _To_Send + 1}, Dest_Neighbor},  
    serialSpreadMessages({SourceIndex,Dest_Neighbor}, M, _To_Send - 1).

% ------ getNeighbors -----------------------------                      %  Generates a list of neighbor indexes of an input index in an NxN grid
getNeighbors(1, _Index) ->                                               %  If N = 1, an NxN grid is not possible...
    [];
getNeighbors(N, Index) ->
    Klist = lists:seq(-N,N),
    Left_Right_List = left_right_neighbors(Klist, N, Index, []),
    Up_Down_List = up_down_neighbors(N, Index),
    _NeighborsList = lists:sort(Left_Right_List ++ Up_Down_List).

% --------------------------------------------------
left_right_neighbors([], _N, _Index, OutputList) ->
    OutputList;
left_right_neighbors([H | T], N, Index, OutputList) ->
    if
        ((1 + H * N) < Index) and (Index < (H + 1) * N) ->
            List = OutputList ++ [Index - 1, Index + 1],
            left_right_neighbors([], N, Index, List);
        (Index == 1 + H * N) ->
            List = OutputList ++ [Index + 1],
            left_right_neighbors([], N, Index, List);
        (Index == H * N) ->
            List = OutputList ++ [Index - 1],
            left_right_neighbors([], N, Index, List);
        true ->
            left_right_neighbors(T, N, Index, [])
    end.
% --------------------------------------------------
up_down_neighbors(N, Index) ->                                           
    if
        (0 < Index - N) and (Index + N =< N * N) ->
            List = [Index - N, Index + N];
        (0 < Index - N) ->
            List = [Index - N];
        (Index + N =< N * N) ->
            List = [Index + N];
        true ->
            List = []
    end,
    List.

