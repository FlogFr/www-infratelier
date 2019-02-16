#!/bin/bash

tmux has-session -t infratelier
if [ $? != 0 ]
then
  tmux -f tmux.conf new-session -s infratelier -c .
fi
tmux attach -t infratelier
