#!/usr/bin/env bash
set -euo pipefail

threads=${THREADS:-4}
connections=${CONNECTIONS:-32}
duration=${DURATION:-10s}
build=${BUILD:-_build/default}
site_port=${SITE_PORT:-18080}

pids=()
trap 'kill "${pids[@]}" 2>/dev/null || true' EXIT

"$build/site/server/main.exe" --port "$site_port" \
  --static "$build/site/client/dist" --metafile "$build/site/client/meta.json" >/dev/null &
pids+=($!)

until curl -sf "http://127.0.0.1:$site_port/_silica/health" >/dev/null; do sleep 0.1; done

cookie() {
  curl -s -o /dev/null -D - "$1" | tr -d '\r' | sed -n 's/^set-cookie: \([^;]*\).*/\1/ip' | head -1
}

site_cookie=$(cookie "http://127.0.0.1:$site_port/")
doc=${DOC:-glossary}
asset=$(curl -s "http://127.0.0.1:$site_port/" | grep -o '/static/client_main-[A-Z0-9]*\.js' | head -1)

echo "wrk -t$threads -c$connections -d$duration; $(nproc) CPUs; $(grep -m1 'model name' /proc/cpuinfo | cut -d: -f2 | sed 's/^ //')"
echo "| target | requests/s | latency avg | transfer/s |"
echo "|---|---|---|---|"
run() {
  local name=$1 url=$2
  shift 2
  local out
  out=$(wrk -t"$threads" -c"$connections" -d"$duration" "$@" "$url")
  local rps lat tr
  rps=$(awk '/Requests\/sec/ {print $2}' <<<"$out")
  lat=$(awk '/Latency/ {print $2; exit}' <<<"$out")
  tr=$(awk '/Transfer\/sec/ {print $2}' <<<"$out")
  echo "| $name | $rps | $lat | $tr |"
}
run landing "http://127.0.0.1:$site_port/" -H "cookie: $site_cookie"
run "doc ($doc)" "http://127.0.0.1:$site_port/docs/$doc" -H "cookie: $site_cookie"
run "asset $asset" "http://127.0.0.1:$site_port$asset"
run "asset $asset (br)" "http://127.0.0.1:$site_port$asset" -H "accept-encoding: br, gzip"
run health "http://127.0.0.1:$site_port/_silica/health"
