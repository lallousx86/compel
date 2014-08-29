#include <stdio.h>
#include "lines.h"
#include "symbol_table.h"
#include <map>
#include <set>

typedef std::multiset<int> IMS_T;

void mset_test()
{
  IMS_T mm;

  for (int i=1;i<=9;i++)
  {
    mm.insert(i);
    mm.insert(i);
  }

  IMS_T::iterator it;
  it = mm.find(5);
  mm.erase(it);
  mm.erase(8);
  for (it=mm.begin();it != mm.end();++it)
  {
    printf("val=%d\n", *it);
  }
  printf("%d of %d\n", mm.count(3), 3);
  //st.add_symbol(
}

int main()
{
  mset_test();
  return 0;
}