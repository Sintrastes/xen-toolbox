#!/bin/sh

# Start the fluid synth server
fluidsynth -K 256 -j -s -i -o "shell.port=9988" &

# Start the scsynth server
export SC_JACK_DEFAULT_INPUTS="system"
export SC_JACK_DEFAULT_OUTPUTS="system"
scsynth -u 57110
