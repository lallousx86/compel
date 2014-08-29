#ifndef __MUTEX_CLASS_09052006__
#define __MUTEX_CLASS_09052006__

#include <windows.h>

class mutex_class
{
private:
  HANDLE _hmutex;
  
  mutex_class &operator=(const mutex_class &rhs) { }
  mutex_class(const mutex_class &rhs) { }

  bool created();
public:
  bool create();
  bool create_named();
  bool acquire();
  bool release();
  bool destroy();
  mutex_class();
  ~mutex_class();
};

class criticalsection_class
{
private:
  CRITICAL_SECTION _ct;
public:
  criticalsection_class();
  ~criticalsection_class();
  void enter();
  void leave();
};

class criticalsection_scope_protect_class
{
private:
  criticalsection_class &_critical;
public:
  criticalsection_scope_protect_class(criticalsection_class &crit);
  ~criticalsection_scope_protect_class();
};

class mutex_scope_protect_class
{
private:
  mutex_class &_mutex_ref;
public:
  mutex_scope_protect_class(mutex_class &mutex);
  ~mutex_scope_protect_class();
};

#endif