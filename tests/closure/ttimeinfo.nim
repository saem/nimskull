discard """
targets: "native"
output: '''
@[2000-01-01T00:00:00Z, 2001-01-01T00:00:00Z, 2002-01-01T00:00:00Z, 2003-01-01T00:00:00Z, 2004-01-01T00:00:00Z, 2005-01-01T00:00:00Z, 2006-01-01T00:00:00Z, 2007-01-01T00:00:00Z, 2008-01-01T00:00:00Z, 2009-01-01T00:00:00Z, 2010-01-01T00:00:00Z, 2011-01-01T00:00:00Z, 2012-01-01T00:00:00Z, 2013-01-01T00:00:00Z, 2014-01-01T00:00:00Z, 2015-01-01T00:00:00Z]
@[2000-01-01T00:00:00Z, 2001-01-01T00:00:00Z, 2002-01-01T00:00:00Z, 2003-01-01T00:00:00Z, 2004-01-01T00:00:00Z, 2005-01-01T00:00:00Z, 2006-01-01T00:00:00Z, 2007-01-01T00:00:00Z, 2008-01-01T00:00:00Z, 2009-01-01T00:00:00Z, 2010-01-01T00:00:00Z, 2011-01-01T00:00:00Z, 2012-01-01T00:00:00Z, 2013-01-01T00:00:00Z, 2014-01-01T00:00:00Z, 2015-01-01T00:00:00Z]
'''
"""

# bug #2073

import sequtils
import times

# 1
proc f(n: int): DateTime =
  initDateTime(1, mJan, n, 0, 0, 0, utc())

echo toSeq(2000 || 2015).map(f)

# 2
echo toSeq(2000 || 2015).map(proc (n: int): DateTime =
  initDateTime(1, mJan, n, 0, 0, 0, utc())
)
