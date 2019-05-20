#!/usr/bin/env python3

import argparse
import re
from os.path import basename


def parse():
    parser = argparse.ArgumentParser(
        description="The gernerator for maxwell protocol api."
    )
    parser.add_argument('--proto_file', required=True, type=argparse.FileType('r'))
    parser.add_argument('--enum_type_names', required=True, nargs='*')
    args = parser.parse_args()
    return args.proto_file, args.enum_type_names


def extract(content, enum_type):
    enum_type_def_pattern = r"enum\s+" + enum_type + "\s+{([^}]+)}"
    enum_type_def_match = re.search(enum_type_def_pattern, content)

    if enum_type_def_match:
        enum_name_pattern = r"([A-Z_0-9]+)\s*=\s*[0-9]+;"
        enum_names = re.findall(enum_name_pattern, enum_type_def_match.group(1))
        return enum_names
    else:
        return []


def output(filename, enum_type_names, enum_names):
    module_name = re.sub(r"([^.]+).proto$", r"\1", basename(filename))

    module_decl_output = f"""-module({module_name})."""
    include_decl_output = f"""-include("{module_name}_pb.hrl")."""

    enum_type_name_prefixs = []
    for enum_type_name in enum_type_names:
        enum_type_name_prefixs.append(
            re.sub(r"([^.]+)_type_t", r"\1", enum_type_name)
        )

    api_export_decls = []
    for enum_type_prefix in enum_type_name_prefixs:
        api_export_decls.append(f"""-export([encode_{enum_type_prefix}/1]).""")
        api_export_decls.append(f"""-export([decode_{enum_type_prefix}/1]).""")
    api_export_decls.append(f"""-export([is_req/1]).""")
    api_export_decls.append(f"""-export([is_rep/1]).""")
    api_export_decls.append(f"""-export([is_msg/1]).""")
    api_export_decls_output = "\n".join(api_export_decls)

    api_impl_defs = []
    for enum_type_prefix in enum_type_name_prefixs:
        api_impl_defs.append(
            f"""encode_{enum_type_prefix}(Msg) ->\n"""
            f"""  MsgTypeValue = get_msg_type_value({enum_type_prefix}_type_t, element(1, Msg)),\n"""
            f"""  EncodedMsgBody = encode_msg_body(Msg),\n"""
            f"""  <<MsgTypeValue:8/little-unsigned-integer-unit:1, EncodedMsgBody/binary>>.\n\n"""
            f"""decode_{enum_type_prefix}(EncodedMsg) ->\n"""
            f"""  <<MsgTypeValue:8/little-unsigned-integer-unit:1, EncodedMsgBody/binary>> = EncodedMsg,\n"""
            f"""  MsgTypeName = get_msg_type_name({enum_type_prefix}_type_t, MsgTypeValue),\n"""
            f"""  decode_msg_body(MsgTypeName, EncodedMsgBody)."""
        )
    api_impl_defs.append(
        f"""is_req(Msg) -> is_req_t(element(1, Msg))."""
    )
    api_impl_defs.append(
        f"""is_rep(Msg) -> is_rep_t(element(1, Msg))."""
    )
    api_impl_defs.append(
        f"""is_msg(Msg) -> is_msg_t(element(1, Msg))."""
    )
    api_impl_defs_output = "\n\n".join(api_impl_defs)

    internal_funs_defs_output0 = \
        f"""get_msg_type_value(EnumTypeName, MsgTypeName) ->\n""" \
        f"""  EnumName = msg_type_name_to_enum_name(MsgTypeName),\n""" \
        f"""  {module_name}_pb:enum_value_by_symbol(EnumTypeName, EnumName).\n\n""" \
        f"""encode_msg_body(Msg) ->\n""" \
        f"""  {module_name}_pb:encode_msg(Msg).\n\n""" \
        f"""get_msg_type_name(EnumTypeName, MsgTypeValue) ->\n""" \
        f"""  EnumName = {module_name}_pb:enum_symbol_by_value(EnumTypeName, MsgTypeValue),\n""" \
        f"""  enum_name_to_msg_type_name(EnumName).\n\n""" \
        f"""decode_msg_body(MsgTypeName, EncodedMsgBody) ->\n""" \
        f"""  {module_name}_pb:decode_msg(EncodedMsgBody, MsgTypeName)."""

    enum_name_to_msg_type_name_defs = []
    for enum_name in enum_names:
        if enum_name[0:7] == "UNKNOWN":
            continue
        enum_name_to_msg_type_name_defs.append(
            f"""enum_name_to_msg_type_name('{enum_name}') -> {str.lower(enum_name)}_t;""")
    enum_name_to_msg_type_name_output = "\n".join(enum_name_to_msg_type_name_defs).rstrip(";") + "."

    msg_type_name_to_enum_name_defs = []
    for enum_name in enum_names:
        if enum_name[0:7] == "UNKNOWN":
            continue
        msg_type_name_to_enum_name_defs.append(
            f"""msg_type_name_to_enum_name({str.lower(enum_name)}_t) -> '{enum_name}';""")
    msg_type_name_to_enum_name_output = "\n".join(msg_type_name_to_enum_name_defs).rstrip(";") + "."

    is_req_t_defs = []
    is_rep_t_defs = []
    is_msg_t_defs = []
    for enum_name in enum_names:
        if enum_name[0:7] == "UNKNOWN":
            continue
        if enum_name[-4:] == '_REQ':
            is_req_t_defs.append(
                f"""is_req_t({str.lower(enum_name)}_t) -> true;""")
        elif enum_name[-4:] == '_REP':
            is_rep_t_defs.append(
                f"""is_rep_t({str.lower(enum_name)}_t) -> true;""")
        elif enum_name[-4:] == '_MSG':
            is_msg_t_defs.append(
                f"""is_msg_t({str.lower(enum_name)}_t) -> true;""")
    is_req_t_defs_output = "\n".join(is_req_t_defs).rstrip(";") + ";\nis_req_t(_) -> false."
    is_rep_t_defs_output = "\n".join(is_rep_t_defs).rstrip(";") + ";\nis_rep_t(_) -> false."
    is_msg_t_defs_output = "\n".join(is_msg_t_defs).rstrip(";") + ";\nis_msg_t(_) -> false."

    internal_funs_defs_output1 = \
        f"""{enum_name_to_msg_type_name_output}\n\n""" \
        f"""{msg_type_name_to_enum_name_output}\n\n""" \
        f"""{is_req_t_defs_output}\n\n""" \
        f"""{is_rep_t_defs_output}\n\n""" \
        f"""{is_msg_t_defs_output}"""

    output = \
        f"""{module_decl_output}\n\n""" \
        f"""{include_decl_output}\n\n""" \
        f"""%% API\n""" \
        f"""{api_export_decls_output}\n\n""" \
        f"""%% =========================================================\n""" \
        f"""%% API implementations\n""" \
        f"""%% =========================================================\n""" \
        f"""{api_impl_defs_output}\n\n""" \
        f"""%% =========================================================\n""" \
        f"""%% Internal functions\n""" \
        f"""%% =========================================================\n""" \
        f"""{internal_funs_defs_output0}\n\n""" \
        f"""{internal_funs_defs_output1}"""

    with open("src/" + module_name + ".erl", "w") as output_file:
        output_file.write(output)


if __name__ == "__main__":
    file, enum_type_names = parse()

    enum_names = []
    content = file.read().replace("\n", "")
    for enum_type in enum_type_names:
        enum_names += extract(content, enum_type)
    enum_names = list(set(enum_names))
    enum_names.sort()

    output(file.name, enum_type_names, enum_names)
