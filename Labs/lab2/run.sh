#!/bin/bash

# shellcheck disable=SC2164
cd fuzz

# Проверяем, сколько аргументов передано
num_args="$#"

# Проверяем, были ли переданы аргументы, и устанавливаем их при необходимости
if [ $num_args -ge 1 ]; then
  arg1="$1"
else
  arg1=""
fi

if [ $num_args -ge 2 ]; then
  arg2="$2"
else
  arg2=""
fi

if [ $num_args -ge 3 ]; then
  arg3="$3"
else
  arg3=""
fi

if [ $num_args -ge 4 ]; then
  arg4="$4"
else
  arg4=""
fi

# Запускаем main.py и передаем аргументы, только если они существуют
args=("$arg1" "$arg2" "$arg3" "$arg4")

# Фильтруем пустые аргументы
filtered_args=()
for arg in "${args[@]}"; do
  if [ -n "$arg" ]; then
    filtered_args+=("$arg")
  fi
done

# Запускаем main.py с аргументами, если они существуют
if [ ${#filtered_args[@]} -gt 0 ]; then
  if command -v python3 &>/dev/null; then
    python3 main.py "${filtered_args[@]}"
  else
    python main.py "${filtered_args[@]}"
  fi
else
  if command -v python3 &>/dev/null; then
    python3 main.py
  else
    python main.py
  fi
fi
