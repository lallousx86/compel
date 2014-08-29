#include "fnc_mm.h"
#include <MMSystem.h>

const static char 
  *s_ns_name = "mm.",
  *s_playwave = "playwave",
  *s_loop = "loop",
  *s_wait = "wait",
  *s_nowait = "nowait",
  *s_stop = "stop",
  *s_dsrender = "dsrender";

const char *fnc_mm::fnc_name = "mm";

fnc_mm::~fnc_mm()
{
  if (_op == op_playwave && _bPlaying)
  {
    ::PlaySoundA(0, 0, SND_PURGE);
  }
}

fnc_mm::fnc_mm(interpreter_t *interpreter, op_e op)
{
  set_interpreter(interpreter);
  _op = op;

  _bPlaying = false;

  switch (_op)
  {
  case op_playwave:
    set_namedesc(s_playwave, "$flag=[wait|nowait|loop|stop] $filename <- plays a wave file");
    set_minmaxargs(2, 2);
    break;
  }
}

fnc_mm::fnc_mm(bool bUseFullNS)
{
  _bUseNameSpace = bUseFullNS;
  _bPlaying = false;
}

parse_errors_e fnc_mm::register_function(interpreter_t *interpreter)
{
  fnc_mm *functions[] =
  {
    new fnc_mm(interpreter, op_playwave)
  };

  for (size_t i=0;i<sizeof(functions)/sizeof(functions[0]);i++)
  {
    std::string fnc_name = functions[i]->_name;
    if (_bUseNameSpace)
      fnc_name = s_ns_name + fnc_name;
    interpreter->add_symbol(fnc_name.c_str(), functions[i]);
  }
  return parse_error_none;
}

parse_errors_e fnc_mm::execute_playwave()
{
  DWORD flag = SND_SYNC;

  interpreter_t *_int = get_interpreter();

  const char *s_flag = _int->get_const_at(1);

  if (strcmp(s_flag, s_loop) == 0)
  {
    flag = SND_ASYNC | SND_LOOP;
    _bPlaying = true;
  }
  else if (strcmp(s_flag, s_stop) == 0)
  {
    flag = SND_PURGE;
    _bPlaying = false;
  }
  else if (strcmp(s_flag, s_nowait) == 0)
    flag = SND_ASYNC;
  else if (strcmp(s_flag, s_wait) == 0)
    flag = SND_SYNC;

  const char *s_fn = _int->evaluate_at(2);

  PlaySoundA(s_fn, 0, flag);

  return parse_error_none;
}

parse_errors_e fnc_mm::execute()
{
  switch (_op)
  {
  case op_playwave:
    return execute_playwave();
  }
  return parse_error_function_expected;
}
