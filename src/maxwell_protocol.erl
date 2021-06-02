-module(maxwell_protocol).

-include("maxwell_protocol_pb.hrl").

%% API
-export([encode_msg/1]).
-export([decode_msg/1]).
-export([is_req/1]).
-export([is_rep/1]).
-export([is_msg/1]).

%% =========================================================
%% API implementations
%% =========================================================
encode_msg(Msg) ->
  MsgTypeValue = get_msg_type_value(msg_type_t, element(1, Msg)),
  EncodedMsgBody = encode_msg_body(Msg),
  <<MsgTypeValue:8/little-unsigned-integer-unit:1, EncodedMsgBody/binary>>.

decode_msg(EncodedMsg) ->
  <<MsgTypeValue:8/little-unsigned-integer-unit:1, EncodedMsgBody/binary>> = EncodedMsg,
  MsgTypeName = get_msg_type_name(msg_type_t, MsgTypeValue),
  decode_msg_body(MsgTypeName, EncodedMsgBody).

is_req(Msg) -> is_req_t(element(1, Msg)).

is_rep(Msg) -> is_rep_t(element(1, Msg)).

is_msg(Msg) -> is_msg_t(element(1, Msg)).

%% =========================================================
%% Internal functions
%% =========================================================
get_msg_type_value(EnumTypeName, MsgTypeName) ->
  EnumName = msg_type_name_to_enum_name(MsgTypeName),
  maxwell_protocol_pb:enum_value_by_symbol(EnumTypeName, EnumName).

encode_msg_body(Msg) ->
  maxwell_protocol_pb:encode_msg(Msg).

get_msg_type_name(EnumTypeName, MsgTypeValue) ->
  EnumName = maxwell_protocol_pb:enum_symbol_by_value(EnumTypeName, MsgTypeValue),
  enum_name_to_msg_type_name(EnumName).

decode_msg_body(MsgTypeName, EncodedMsgBody) ->
  maxwell_protocol_pb:decode_msg(EncodedMsgBody, MsgTypeName).

enum_name_to_msg_type_name('ADD_ROUTE_MSG') -> add_route_msg_t;
enum_name_to_msg_type_name('ADD_ROUTE_REP') -> add_route_rep_t;
enum_name_to_msg_type_name('ADD_ROUTE_REQ') -> add_route_req_t;
enum_name_to_msg_type_name('AUTH_REP') -> auth_rep_t;
enum_name_to_msg_type_name('AUTH_REQ') -> auth_req_t;
enum_name_to_msg_type_name('DELETE_ROUTE_MSG') -> delete_route_msg_t;
enum_name_to_msg_type_name('DELETE_ROUTE_REP') -> delete_route_rep_t;
enum_name_to_msg_type_name('DELETE_ROUTE_REQ') -> delete_route_req_t;
enum_name_to_msg_type_name('DELETE_TOPICS_REP') -> delete_topics_rep_t;
enum_name_to_msg_type_name('DELETE_TOPICS_REQ') -> delete_topics_req_t;
enum_name_to_msg_type_name('DO2_REP') -> do2_rep_t;
enum_name_to_msg_type_name('DO2_REQ') -> do2_req_t;
enum_name_to_msg_type_name('DO_REP') -> do_rep_t;
enum_name_to_msg_type_name('DO_REQ') -> do_req_t;
enum_name_to_msg_type_name('ERROR2_REP') -> error2_rep_t;
enum_name_to_msg_type_name('ERROR_REP') -> error_rep_t;
enum_name_to_msg_type_name('OK2_REP') -> ok2_rep_t;
enum_name_to_msg_type_name('OK_REP') -> ok_rep_t;
enum_name_to_msg_type_name('PING_REP') -> ping_rep_t;
enum_name_to_msg_type_name('PING_REQ') -> ping_req_t;
enum_name_to_msg_type_name('PULL_REP') -> pull_rep_t;
enum_name_to_msg_type_name('PULL_REQ') -> pull_req_t;
enum_name_to_msg_type_name('PULL_ROUTES_REP') -> pull_routes_rep_t;
enum_name_to_msg_type_name('PULL_ROUTES_REQ') -> pull_routes_req_t;
enum_name_to_msg_type_name('PUSH_REP') -> push_rep_t;
enum_name_to_msg_type_name('PUSH_REQ') -> push_req_t;
enum_name_to_msg_type_name('PUSH_ROUTES_REP') -> push_routes_rep_t;
enum_name_to_msg_type_name('PUSH_ROUTES_REQ') -> push_routes_req_t;
enum_name_to_msg_type_name('REGISTER_BACKEND_REP') -> register_backend_rep_t;
enum_name_to_msg_type_name('REGISTER_BACKEND_REQ') -> register_backend_req_t;
enum_name_to_msg_type_name('REGISTER_FRONTEND_REP') -> register_frontend_rep_t;
enum_name_to_msg_type_name('REGISTER_FRONTEND_REQ') -> register_frontend_req_t;
enum_name_to_msg_type_name('RESOLVE_BACKEND_REP') -> resolve_backend_rep_t;
enum_name_to_msg_type_name('RESOLVE_BACKEND_REQ') -> resolve_backend_req_t;
enum_name_to_msg_type_name('RESOLVE_FRONTEND_REP') -> resolve_frontend_rep_t;
enum_name_to_msg_type_name('RESOLVE_FRONTEND_REQ') -> resolve_frontend_req_t;
enum_name_to_msg_type_name('RESOLVE_IP_REP') -> resolve_ip_rep_t;
enum_name_to_msg_type_name('RESOLVE_IP_REQ') -> resolve_ip_req_t;
enum_name_to_msg_type_name('UNWATCH_REP') -> unwatch_rep_t;
enum_name_to_msg_type_name('UNWATCH_REQ') -> unwatch_req_t;
enum_name_to_msg_type_name('WATCH_REP') -> watch_rep_t;
enum_name_to_msg_type_name('WATCH_REQ') -> watch_req_t.

msg_type_name_to_enum_name(add_route_msg_t) -> 'ADD_ROUTE_MSG';
msg_type_name_to_enum_name(add_route_rep_t) -> 'ADD_ROUTE_REP';
msg_type_name_to_enum_name(add_route_req_t) -> 'ADD_ROUTE_REQ';
msg_type_name_to_enum_name(auth_rep_t) -> 'AUTH_REP';
msg_type_name_to_enum_name(auth_req_t) -> 'AUTH_REQ';
msg_type_name_to_enum_name(delete_route_msg_t) -> 'DELETE_ROUTE_MSG';
msg_type_name_to_enum_name(delete_route_rep_t) -> 'DELETE_ROUTE_REP';
msg_type_name_to_enum_name(delete_route_req_t) -> 'DELETE_ROUTE_REQ';
msg_type_name_to_enum_name(delete_topics_rep_t) -> 'DELETE_TOPICS_REP';
msg_type_name_to_enum_name(delete_topics_req_t) -> 'DELETE_TOPICS_REQ';
msg_type_name_to_enum_name(do2_rep_t) -> 'DO2_REP';
msg_type_name_to_enum_name(do2_req_t) -> 'DO2_REQ';
msg_type_name_to_enum_name(do_rep_t) -> 'DO_REP';
msg_type_name_to_enum_name(do_req_t) -> 'DO_REQ';
msg_type_name_to_enum_name(error2_rep_t) -> 'ERROR2_REP';
msg_type_name_to_enum_name(error_rep_t) -> 'ERROR_REP';
msg_type_name_to_enum_name(ok2_rep_t) -> 'OK2_REP';
msg_type_name_to_enum_name(ok_rep_t) -> 'OK_REP';
msg_type_name_to_enum_name(ping_rep_t) -> 'PING_REP';
msg_type_name_to_enum_name(ping_req_t) -> 'PING_REQ';
msg_type_name_to_enum_name(pull_rep_t) -> 'PULL_REP';
msg_type_name_to_enum_name(pull_req_t) -> 'PULL_REQ';
msg_type_name_to_enum_name(pull_routes_rep_t) -> 'PULL_ROUTES_REP';
msg_type_name_to_enum_name(pull_routes_req_t) -> 'PULL_ROUTES_REQ';
msg_type_name_to_enum_name(push_rep_t) -> 'PUSH_REP';
msg_type_name_to_enum_name(push_req_t) -> 'PUSH_REQ';
msg_type_name_to_enum_name(push_routes_rep_t) -> 'PUSH_ROUTES_REP';
msg_type_name_to_enum_name(push_routes_req_t) -> 'PUSH_ROUTES_REQ';
msg_type_name_to_enum_name(register_backend_rep_t) -> 'REGISTER_BACKEND_REP';
msg_type_name_to_enum_name(register_backend_req_t) -> 'REGISTER_BACKEND_REQ';
msg_type_name_to_enum_name(register_frontend_rep_t) -> 'REGISTER_FRONTEND_REP';
msg_type_name_to_enum_name(register_frontend_req_t) -> 'REGISTER_FRONTEND_REQ';
msg_type_name_to_enum_name(resolve_backend_rep_t) -> 'RESOLVE_BACKEND_REP';
msg_type_name_to_enum_name(resolve_backend_req_t) -> 'RESOLVE_BACKEND_REQ';
msg_type_name_to_enum_name(resolve_frontend_rep_t) -> 'RESOLVE_FRONTEND_REP';
msg_type_name_to_enum_name(resolve_frontend_req_t) -> 'RESOLVE_FRONTEND_REQ';
msg_type_name_to_enum_name(resolve_ip_rep_t) -> 'RESOLVE_IP_REP';
msg_type_name_to_enum_name(resolve_ip_req_t) -> 'RESOLVE_IP_REQ';
msg_type_name_to_enum_name(unwatch_rep_t) -> 'UNWATCH_REP';
msg_type_name_to_enum_name(unwatch_req_t) -> 'UNWATCH_REQ';
msg_type_name_to_enum_name(watch_rep_t) -> 'WATCH_REP';
msg_type_name_to_enum_name(watch_req_t) -> 'WATCH_REQ'.

is_req_t(add_route_req_t) -> true;
is_req_t(auth_req_t) -> true;
is_req_t(delete_route_req_t) -> true;
is_req_t(delete_topics_req_t) -> true;
is_req_t(do2_req_t) -> true;
is_req_t(do_req_t) -> true;
is_req_t(ping_req_t) -> true;
is_req_t(pull_req_t) -> true;
is_req_t(pull_routes_req_t) -> true;
is_req_t(push_req_t) -> true;
is_req_t(push_routes_req_t) -> true;
is_req_t(register_backend_req_t) -> true;
is_req_t(register_frontend_req_t) -> true;
is_req_t(resolve_backend_req_t) -> true;
is_req_t(resolve_frontend_req_t) -> true;
is_req_t(resolve_ip_req_t) -> true;
is_req_t(unwatch_req_t) -> true;
is_req_t(watch_req_t) -> true;
is_req_t(_) -> false.

is_rep_t(add_route_rep_t) -> true;
is_rep_t(auth_rep_t) -> true;
is_rep_t(delete_route_rep_t) -> true;
is_rep_t(delete_topics_rep_t) -> true;
is_rep_t(do2_rep_t) -> true;
is_rep_t(do_rep_t) -> true;
is_rep_t(error2_rep_t) -> true;
is_rep_t(error_rep_t) -> true;
is_rep_t(ok2_rep_t) -> true;
is_rep_t(ok_rep_t) -> true;
is_rep_t(ping_rep_t) -> true;
is_rep_t(pull_rep_t) -> true;
is_rep_t(pull_routes_rep_t) -> true;
is_rep_t(push_rep_t) -> true;
is_rep_t(push_routes_rep_t) -> true;
is_rep_t(register_backend_rep_t) -> true;
is_rep_t(register_frontend_rep_t) -> true;
is_rep_t(resolve_backend_rep_t) -> true;
is_rep_t(resolve_frontend_rep_t) -> true;
is_rep_t(resolve_ip_rep_t) -> true;
is_rep_t(unwatch_rep_t) -> true;
is_rep_t(watch_rep_t) -> true;
is_rep_t(_) -> false.

is_msg_t(add_route_msg_t) -> true;
is_msg_t(delete_route_msg_t) -> true;
is_msg_t(_) -> false.