#ifndef __FUNCTION__01232006__
#define __FUNCTION__01232006__

#include "fwd.h"
#include "symbol.h"

class function_t : public symbol_t
{
  friend class function_helper_t;
private:
  void basic_init();
  interpreter_t *_interpreter;

protected:
  std::string _name, _desc;
  size_t _minargs, _maxargs;
  int _flags;
public:

  function_t();

  interpreter_t *get_interpreter() const;

  void init(
    const char *fncname, 
    const char *desc, 
    size_t minargs,
    size_t maxargs,
    interpreter_t *,
    int = function_flag_none);

  function_t(
    const char *fncname, 
    const char *desc, 
    int minargs,
    int maxargs,
    interpreter_t * = 0,
    int = function_flag_none);

  function_t(const function_t &rhs);
 
  virtual parse_errors_e prepare(size_t passno, int &starting_or_failing_line);
  virtual parse_errors_e execute() = 0;
  virtual parse_errors_e register_function(interpreter_t *);

  void set_interpreter(interpreter_t *interpreter);
  void set_namedesc(const char *name, const char *desc);
  void set_minmaxargs(const size_t minargs, const size_t maxargs);
  void set_flags(int flags);

  size_t get_minargs() const;
  size_t get_maxargs() const;
  const char *get_name() const;
  const int get_flags() const;
  virtual ~function_t();
};

#endif