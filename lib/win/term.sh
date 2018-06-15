#------------------------------------------------------------------------------
# generate TermInfo.d from /usr/include/term.h
# afb 5/88
#------------------------------------------------------------------------------

	PATH=/bin:/usr/bin	# be sure to call commands we know about
	tmp=/tmp/ti$$
	tmp2=/tmp/ti_$$

trap "rm -f $tmp $tmp2" 0 1 2 3 15

tr -d ',;' </usr/include/term.h \
| awk '
BEGIN			{ copy = 0; part = 0;
			  bools = 0; ints = 0; strings = 0;
			  upper="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
			  lower="abcdefghijklmnopqrstuvwxyz"
			}
/struct strs {/		{ part = 1 }
/struct strs2 {/	{ part = 2 }
/struct term {/		{ part = 3 }
/charptr/		{ if (part) copy = 2 }
/^[ 	]*char[ 	]*/ { if (part) copy = 2 }
/short/			{ if (part == 3) { part = 4; copy = 2 } }
/struct strs strs/	{ copy = 0 }
/^}/			{ copy = 0 }

			{ if (copy == 2)
				copy = 1;
			  else if (copy == 1)
			  {	split($1, comp, "_");
				i = 2; out = comp[1];
				while (comp[i] != "")
				{	head = substr(comp[i], 1, 1);
					tail = substr(comp[i], 2);
					letter = index(lower, head);
					if (letter)
						head = substr(upper, letter, 1);
					out = out head tail;
					++ i;
				}
				if (part == 1 || part == 2)
				{	printf "%s: String;\n", out
					++ strings;
				}
				else if (part == 3)
				{	printf "%s: BOOLEAN;\n", out
					++ bools;
				}
				else if (part == 4)
				{	printf "%s: INTEGER;\n", out
					++ ints;
				}
			  }
			}
END			{	printf "strings = %d;bools = %d;ints = %d;\n", \
					strings, bools, ints >"'$tmp2'"
			}
' >$tmp

ex TermInfo.t >/dev/null <<eof
/Term =/
+
r $tmp
/CONST/
r $tmp2
1,\$!m2b
w! TermInfo.d
eof
