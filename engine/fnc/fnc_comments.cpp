#include "fnc_comments.h"
#include "../interpreter_helper.h"

static const char 
  *s_cmd_slc_hash = "#",
  *s_cmd_slc_fwd2 = "//",
  *s_cmd_mlc1_beg = "/*",
  *s_cmd_mlc1_end = "*/";

const char *fnc_comments::fnc_name = "comments";

fnc_comments::fnc_comments(interpreter_t *interpreter, op_e op)
{
  _op = op;

  switch (_op)
  {
  case op_slc_hash:
    set_namedesc(s_cmd_slc_hash, "# this is a comment line");
    set_minmaxargs(0, 0);
    break;
  case op_slc_fwd2:
    set_namedesc(s_cmd_slc_fwd2, "// this is a comment line");
    set_minmaxargs(0, 0);
    break;
  case op_mlc1_beg:
    set_namedesc(s_cmd_mlc1_beg, "/* This is multi-line comment begin");
    set_minmaxargs(0, 0);
    break;
  case op_mlc1_end:
    set_namedesc(s_cmd_mlc1_end, "this is multi-line comment end */");
    set_minmaxargs(0, 0);
    break;
  }
  set_interpreter(interpreter);
}

fnc_comments::fnc_comments()
{
}

parse_errors_e fnc_comments::execute()
{
  return parse_error_line_is_comment;
}

parse_errors_e fnc_comments::prepare(size_t passno, int &start_or_failing_line)
{
  if (passno != 2)
    return parse_error_none;

  interpreter_t *_int = get_interpreter();

  interpreter_helper_t inthlp(_int);

  lines_container_type_t &lines = inthlp.get_interpreter_lines().get_string_list();

  compel_string_tokenizer_t slp;

	lines_container_type_t::iterator it = lines.begin();
  size_t lineno = start_or_failing_line;

	std::advance(it, lineno);

	bool b_in_mlc1 = false;

  for (;
    it != lines.end();++it, lineno++)
  {
    std::string &line = *it;
    size_t pcount = slp.parse(line.c_str());
    if (pcount < 1)
      continue;

    const char *cmd = slp.get_string(0);
    const char *c_line = line.c_str();

    if (b_in_mlc1 == false)
    {
      // found a single hash?
      if (strncmp(cmd, s_cmd_slc_hash, 1) == 0)
      {
        if (strcmp(cmd, s_cmd_slc_hash) == 0)
          continue;
        line = s_cmd_slc_hash + std::string(" ") + line;
        continue;
      }

      // found a 2-fwd?
      else if (strncmp(cmd, s_cmd_slc_fwd2, 2) == 0)
      {
        if (strcmp(cmd, s_cmd_slc_fwd2) == 0)
          continue;
        line = s_cmd_slc_fwd2 + std::string(" ") + line;
        continue;
      }
      else if (strncmp(cmd, s_cmd_mlc1_beg, 2) == 0)
      {
        // same line has begin and end comments
        if (strstr(c_line, s_cmd_mlc1_end) == 0)
          b_in_mlc1 = true;

        if (strcmp(cmd, s_cmd_mlc1_beg) == 0)
          continue;
        line = s_cmd_slc_hash + std::string(" ") + line;
      }
      // not in comment - and found closing comment!
      else if (strncmp(cmd, s_cmd_mlc1_end, 2) == 0)
      {
        start_or_failing_line = (int) lineno;
        return parse_error_wrong_context;
      }
    }
    else
    {
      // comment the line
      line = s_cmd_slc_hash + std::string(" ") + line;
      // found end of mlc1?
      if (strstr(c_line, s_cmd_mlc1_end) != 0)
        b_in_mlc1 = false;
      continue;
    }
  }

  return parse_error_none;
}

parse_errors_e fnc_comments::register_function(interpreter_t *interpreter)
{
  fnc_comments *fnc_mlc1 = new fnc_comments(interpreter, op_mlc1_beg);

  fnc_comments *functions[] =
  {
    new fnc_comments(interpreter, op_slc_hash),
    new fnc_comments(interpreter, op_slc_fwd2),
    new fnc_comments(interpreter, op_mlc1_end),
    fnc_mlc1
  };

  // pass2 - we need to run before SCOPES, so that the code isn't patched
  // we need to run after INCLUDE so that we comment inline
  fnc_mlc1->set_flags(fnc_mlc1->get_flags() | function_flag_pass2);

  for (size_t i=0;i<sizeof(functions)/sizeof(functions[0]);i++)
    interpreter->add_symbol(functions[i]->_name.c_str(), functions[i]);

  return parse_error_none;
}