__author__="Matias Carrasco Kind"
#!/usr/bin/env python
import os,sys

files="http://lcdm.astro.illinois.edu/static/code/mlz/astro_bigdata.tar.gz"
os.system('wget '+files+ ' -O astro_bigdata.tar.gz')
os.system('tar -zxf astro_bigdata.tar.gz')
os.system('mv *.css *.ipynb /home/cse-user/astro/.')
os.system('mkdir -p /home/cse-user/astro/Figures')
os.system('cp -R Figures/* /home/cse-user/astro/Figures')
os.system('rm -rf Figures/')
os.system('mv *.npy *.mlz *.header /home/cse-user/astro/data')
os.system('rm -f astro_bigdata.tar.gz')

print 
print "******************************"
print "*   Move to the astro/ folder      "
print "*   $ cd /home/cse-user/astro   "
print "*   $ ipython notebook             "
print "******************************"
print
