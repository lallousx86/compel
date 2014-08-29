#include "function.h"

void function_t::basic_init()
{
  set_sym_kind(symbol_function);
  set_interpreter(0);
  _flags = function_flag_none;
}

function_t::function_t()
{
  basic_init();
}

void function_t::init(const char *fncname, 
                      const char *desc, 
                      size_t minargs,
                      size_t maxargs,
                      interpreter_t *interpreter,
                      int flags)
{
  basic_init();
  _minargs = minargs;
  _maxargs = maxargs;
  _name = fncname;
  _desc = desc;
  set_interpreter(interpreter);
  _flags = flags;
}

function_t::function_t(
  const char *fncname, 
  const char *desc, 
  int minargs,
  int maxargs,
  interpreter_t *interpreter,
  int flags)
{
  init(fncname, desc, minargs, maxargs, interpreter, flags);
}

interpreter_t *function_t::get_interpreter() const
{
  return _interpreter;
}

function_t::function_t(const function_t &rhs)
{
  init(
    rhs._name.c_str(), 
    rhs._desc.c_str(), 
    rhs._minargs, 
    rhs._maxargs, 
    rhs._interpreter, 
    rhs._flags
    );
}

void function_t::set_interpreter(interpreter_t *interpreter)
{
  _interpreter = interpreter;
}

void function_t::set_namedesc(const char *name, const char *desc)
{
  _name = name;
  _desc = desc;
}

void function_t::set_minmaxargs(const size_t minargs, const size_t maxargs)
{
  _minargs = minargs;
  _maxargs = maxargs;
}

const int function_t::get_flags() const
{
  return _flags;
}

void function_t::set_flags(int flags)
{
  _flags = flags;
}

size_t function_t::get_minargs() const
{
  return _minargs;
}

size_t function_t::get_maxargs() const
{
  return _maxargs;
}

parse_errors_e function_t::prepare(size_t passno, int &starting_or_failing_line)
{
  return parse_error_none;
}

parse_errors_e function_t::register_function(interpreter_t *interpreter)
{
  return parse_error_none;
}

function_t::~function_t() 
{ 

}

const char *function_t::get_name() const
{
  return _name.c_str();
}