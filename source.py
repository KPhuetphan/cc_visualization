#!/usr/bin/env python

def do_root_cause_analysis(log, net, initial_marking, final_marking):
    import tkinter
    import traceback
    import os
    from pm4py.objects.log.importer.xes import importer as xes_importer
    from pm4py.algo.conformance.tokenreplay import algorithm as token_based_replay
    from pm4py.algo.conformance.tokenreplay.diagnostics import root_cause_analysis
    from pm4py.visualization.decisiontree import visualizer as dt_vis
    
    result = {"trans": {}, "act": {}}

    try:
        parameters_tbr = {token_based_replay.Variants.TOKEN_REPLAY.value.Parameters.DISABLE_VARIANTS: True, token_based_replay.Variants.TOKEN_REPLAY.value.Parameters.ENABLE_PLTR_FITNESS: True}
        replayed_traces, place_fitness, trans_fitness, unwanted_activities = token_based_replay.apply(log, net, initial_marking, final_marking, parameters=parameters_tbr)

        string_attributes = ["org:resource"]
        numeric_attributes = []
        parameters = {"string_attributes": string_attributes, "numeric_attributes": numeric_attributes}

        trans_root_cause = root_cause_analysis.diagnose_from_trans_fitness(log, trans_fitness, parameters=parameters)
        act_root_cause = root_cause_analysis.diagnose_from_notexisting_activities(log, unwanted_activities, parameters=parameters)

        for trans in trans_root_cause:
            clf = trans_root_cause[trans]["clf"]
            feature_names = trans_root_cause[trans]["feature_names"]
            classes = trans_root_cause[trans]["classes"]
            # visualization could be called
            gviz = dt_vis.apply(clf, feature_names, classes)
            result["trans"][str(trans)] = gviz
            
        for act in act_root_cause:
            clf = act_root_cause[act]["clf"]
            feature_names = act_root_cause[act]["feature_names"]
            classes = act_root_cause[act]["classes"]
            # visualization could be called
            gviz = dt_vis.apply(clf, feature_names, classes)
            result["act"][str(act)] = gviz
    except:
        root = tkinter.Tk()
        root.overrideredirect(1)
        root.withdraw()
        tkinter.messagebox.showerror(title="Root Cause Analysis", message=str(traceback.format_exc()))

    return result



def render_align_table(log, aligned_traces, parameters=None):
    """
    Gets the alignment table visualization from the alignments output

    Parameters
    -------------
    log
        Event log
    aligned_traces
        Aligned traces
    parameters
        Parameters of the algorithm

    Returns
    -------------
    gviz
        Graphviz object
    """
    from graphviz import Source
    import tempfile
    from pm4py.statistics.variants.log import get as variants_get
    import graphviz
    if parameters is None:
        parameters = {}

    variants_idx_dict = variants_get.get_variants_from_log_trace_idx(log, parameters=parameters)

    variants_idx_list = []
    for variant in variants_idx_dict:
        variants_idx_list.append((variant, variants_idx_dict[variant]))
    variants_idx_list = sorted(variants_idx_list, key=lambda x: len(x[1]), reverse=True)

    table_alignments_list = ["digraph {\n", "tbl [\n", "shape=plaintext\n", "label=<\n"]
    table_alignments_list.append("<table border='0' cellborder='1' color='black' cellspacing='0'>\n")

    table_alignments_list.append("<tr><td>Variant</td><td>Trace</td></tr>\n")

    for index, variant in enumerate(variants_idx_list):
        al_tr = aligned_traces[variant[1][0]]
        table_alignments_list.append("<tr>")
        table_alignments_list.append(
            "<td><font point-size='9'>Variant " + str(index + 1) + " (" + str(
                len(variant[1])) + " occurrences)</font></td>")
        table_alignments_list.append("<td><font point-size='6'><table border='0'><tr>")
        for move in al_tr['alignment']:
            move_descr = str(move[1]).replace(">", "&gt;")
            if not move[1]:
                pass
            elif move[0] == move[1]:
                table_alignments_list.append("<td bgcolor=\"green\">" + move_descr + "</td>")
            elif move[1] == ">>":
                move_descr = str(move[0]).replace(">", "&gt;")
                table_alignments_list.append("<td bgcolor=\"violet\">" + move_descr + "</td>")
            elif move[0] == ">>":
                table_alignments_list.append("<td bgcolor=\"red\">" + move_descr + "</td>")
        table_alignments_list.append("</tr></table></font></td>")
        table_alignments_list.append("</tr>")

    table_alignments_list.append("</table>\n")
    table_alignments_list.append(">];\n")
    table_alignments_list.append("}\n")

    table_alignments = "".join(table_alignments_list)

    filename = tempfile.NamedTemporaryFile(suffix='.gv')

    gviz = Source(table_alignments, filename=filename.name)
    gviz.format = "png"

    return gviz

