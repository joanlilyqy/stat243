#!/bin/bash

#### Stat 243 Fall 2012
#### Problem Set 1

#### 2.(a)
# Find R jobs and CPU usage on remote machines
function remoteRJobs () {
		if [ $# == "2" ]
		then 
		   echo "The top ${2} %CPU usage of R jobs running on ${1}:" 
		   echo "PID	UID	%CPU	CMD"
		   ssh $1 ps -C R -o pid,user,%cpu,comm --sort=-%cpu | grep -v PID | head -n $2
		   echo ""
		elif [ $# == "1" ]
		then 
		   echo "The %CPU usage of R jobs running on ${1}:" 
		   echo "PID	UID	%CPU	CMD"
		   ssh $1 ps -C R -o pid,user,%cpu,comm --sort=-%cpu | grep -v PID
		   echo ""
		else
			 echo "ERROR: ***************************"
		   echo "remoteRJobs MACHINE [number]"
			 echo ""
		fi 
}

echo "------------------------------2.(a)-----------------------------"
echo "remoteRJobs - query a machine to find all R jobs running"
echo "              and returns the CPU usage of those jobs with"
echo "              the most intensive jobs listed first"
echo ""
echo "remoteRJobs MACHINE [number]"
echo "MACHINE				remote machine name"
echo "[number]			number of jobs listed"
echo "****************************************************************"
echo "Test Case 1: remoteRJobs beren 10"
remoteRJobs beren 10
echo "Test Case 2: remoteRJobs beren"
remoteRJobs beren
echo "Test Case 3: remoteRJobs"
remoteRJobs
echo "----------------------------------------------------------------"
echo ""


#### 2.(b)
# Extend 2.(a) to add up the CPU and memory use of all R jobs
function mysum () {
		sum=0
		for num in $(cat $1)
		do
			sum=$((sum+=num))
		done
}

function remoteRJobs2 () {
		if [ $# == "2" ]
		then 
		   echo "The top ${2} %CPU usage of R jobs running on ${1}:" 
		   echo "PID	UID	%CPU	%MEM	CMD"
		   ssh $1 ps -C R -o pid,user,pcpu,pmem,comm --sort=-pcpu | grep -v "%CPU" | head -n $2
		   
		   cpunum=$(ssh $1 grep processor /proc/cpuinfo | wc -l)
		   # calculate cpu and memory usage
		   ssh $1 ps -C R -o pcpu --sort=-pcpu | grep -v "%CPU" | head -n $2 | sed 's/ //' | sed 's/\.//' | sed 's/^0//' > cpu.txt
		   ssh $1 ps -C R -o pmem --sort=-pmem | grep -v "%MEM" | head -n $2 | sed 's/ //' | sed 's/\.//' | sed 's/^0//' > mem.txt
		   mysum cpu.txt;sum=$((sum/=10));sum=$((sum/=cpunum))
		   echo "The top ${2} total %CPU used by R jobs on ${1} is ${sum}% for ${cpunum} CPUs"
		   mysum mem.txt;sum=$((sum/=10))
		   echo "The top ${2} total %MEM used by R jobs on ${1} is ${sum}%"
		   
		   echo ""
		elif [ $# == "1" ]
		then 
		   echo "The %CPU usage of R jobs running on ${1}:" 
		   echo "PID	UID	%CPU	%MEM	CMD"
		   ssh $1 ps -C R -o pid,user,pcpu,pmem,comm --sort=-pcpu | grep -v "%CPU"
		   
		   cpunum=$(ssh $1 grep processor /proc/cpuinfo | wc -l)
		   # calculate cpu and memory usage
		   ssh $1 ps -C R -o pcpu --sort=-pcpu | grep -v "%CPU" | sed 's/ //' | sed 's/\.//' | sed 's/^0//' > cpu.txt
		   ssh $1 ps -C R -o pmem --sort=-pmem | grep -v "%MEM" | sed 's/ //' | sed 's/\.//' | sed 's/^0//' > mem.txt
		   mysum cpu.txt;sum=$((sum/=10));sum=$((sum/=cpunum))
		   echo "The total %CPU used by R jobs on ${1} is ${sum}% for ${cpunum} CPUs"
		   mysum mem.txt;sum=$((sum/=10))
		   echo "The total %MEM used by R jobs on ${1} is ${sum}%"
		   
		   echo ""
		else
			 echo "ERROR: ***************************"
		   echo "remoteRJobs MACHINE [number]"
			 echo ""
		fi 
}

echo "------------------------------2.(b)-----------------------------"
echo "remoteRJobs2 - query a machine to find all R jobs running"
echo "               and returns the CPU usage of those jobs with"
echo "               the most intensive jobs listed first;"
echo "               also add up CPU and memory use of these jobs"
echo ""
echo "remoteRJobs2 MACHINE [number]"
echo "MACHINE				remote machine name"
echo "[number]			number of jobs listed"
echo "****************************************************************"
echo "Test Case 1: remoteRJobs2 beren 10"
remoteRJobs2 beren 10
echo "Test Case 2: remoteRJobs2 beren"
remoteRJobs2 beren
echo "Test Case 3: remoteRJobs2 beren arwen 10"
remoteRJobs2 beren arwen 10
echo "----------------------------------------------------------------"
echo ""

rm -f *.txt


