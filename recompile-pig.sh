cd /home/cse-user/Code/pig-0.12.1
sudo JAVA_HOME=/usr/local/Java/jdk1.8.0_05/ ant -Dhadoopversion=23

sudo cp -r /home/cse-user/Code/pig-0.12.1 /home/hduser/Code
cd /home/hduser/Code/pig-0.12.1
sudo chown -R hduser:hadoop .


