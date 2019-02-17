import os
import subprocess

n=10000
a = subprocess.check_output(["./../build/zeta0",str(n)])
print(a)
#os.system("time ./zeta0 " + "5")
