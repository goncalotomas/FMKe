-define(DEFAULT_ANTIDOTE_PORT, "[8087]").
-define(DEFAULT_ANTIDOTE_ADDRESS, "[127.0.0.1]").
-define(VAR_ANTIDOTE_PB_PID, antidote_pb_pid).
-define(VAR_ANTIDOTE_PB_ADDRESS, antidote_pb_address).
-define(VAR_ANTIDOTE_PB_PORT, antidote_pb_port).

%% Useful shortcut macros
-define (BCOUNTER, antidote_crdt_bcounter).
-define (counter, antidote_crdt_counter).
-define (GSET, antidote_crdt_gset).
-define (LWWREG, antidote_crdt_lwwreg).
-define (MAP, antidote_crdt_gmap).
-define (NESTED_MAP, antidote_crdt_gmap).
-define (MVREG, antidote_crdt_mvreg).
-define (ORSET, antidote_crdt_orset).
-define (RGA, antidote_crdt_rga).