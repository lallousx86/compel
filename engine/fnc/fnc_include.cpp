#include "fnc_include.h"
#include "../interpreter_helper.h"

static const char *s_cmd_include = "include";
static const char *s_mod_once    = "once";

static const int PAR_INCFILE  = 1;
static const int PAR_MODIFIER = 2;

const char *fnc_include::fnc_name = "include";

// counts the referenced files
str_sizet_map_t fnc_include::_files_ref;

fnc_include::fnc_include(interpreter_t *interpreter)
{
  set_namedesc(s_cmd_include, "Includes another file");
  set_minmaxargs(1, 0);
  set_interpreter(interpreter);
}

fnc_include::fnc_include()
{

}

parse_errors_e fnc_include::execute()
{
  // should become a void line anyway, and should not be executed!
  return parse_error_none;
}

parse_errors_e fnc_include::register_function(interpreter_t *interpreter)
{
  fnc_include *f_include = new fnc_include(interpreter);
  f_include->set_flags(f_include->get_flags() | function_flag_pass2);
  interpreter->add_symbol(s_cmd_include, f_include);

  return parse_error_none;
}

// Get the reference count for a given file
// If the file was never referenced then we return 0 and initiate the ref counter
size_t &fnc_include::get_file_ref_count(const char *fn)
{
  str_sizet_map_t::iterator it;
  it = _files_ref.find(fn);
  if (it == _files_ref.end())
  {
    str_sizet_map_t::_Pairib pib = _files_ref.insert(str_sizet_map_t::value_type(fn, 0));
    it = pib.first;
  }
  return it->second;
}

parse_errors_e fnc_include::prepare(size_t passno, int &start_or_failing_line)
{
  // We work at before last pass
  if (passno != cc_nbpasses-1)
    return parse_error_none;

  interpreter_t *_int = get_interpreter();

  interpreter_helper_t inthlp(_int);

  lines_t &lines_class = inthlp.get_interpreter_lines();
  lines_container_type_t &lines = lines_class.get_string_list();

  compel_string_tokenizer_t slp;

  lines_container_type_t::iterator it;

  bool bRepeat = true;

  while (bRepeat)
  {
    bRepeat = false;
    size_t lineno = start_or_failing_line;

		it = lines.begin();
		std::advance(it, lineno);

    for (;it != lines.end();++it, lineno++)
    {
      std::string &line = *it;
      size_t pcount = slp.parse((char *)line.c_str());
      if (pcount <= 1)
        continue;

      if (stricmp(slp.get_string(0), s_cmd_include) != 0)
        continue;

      // better to delete through the class
      // remove the INCLUDE line
      lines_class.delete_line(lineno);

      bool bOnce = false;
      if (pcount > PAR_MODIFIER && 
          stricmp(slp.get_string(PAR_MODIFIER), s_mod_once) == 0)
      {
        bOnce = true;
      }

      const char *inc_fn = slp.get_string(PAR_INCFILE);
      size_t &ref = get_file_ref_count(inc_fn);

      // if include once, then we will put it in the list
      if (ref < 1)
      {
        // increment reference
        if (bOnce)
          ref++;

        lines_t inc_file;
        if (!inc_file.read_from_file(inc_fn))
          return parse_error_file_not_found;

        lineno = lines_class.merge_lines(inc_file, lineno);
      }
      bRepeat = true;
      break;
    }
  }
  return parse_error_none;
}