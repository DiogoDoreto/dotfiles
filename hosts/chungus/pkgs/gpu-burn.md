# gpu-burn

Stress test the GPU

```sh
nix-build gpu-burn.nix 
./result/bin/gpu_burn -l
./result/bin/gpu_burn -i 0 -tc 300
```

On a side terminal:

```sh
nvidia-smi --query-gpu=temperature.gpu,power.draw,utilization.gpu,clocks.sm,fan.speed --format=csv -l 1
```
