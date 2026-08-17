# o9Platform / Live Server local development helpers.
#
# Adapted from the CoreDev wiki's WSL guide:
#   CoreDev.wiki/WebApi-%2D-Index/Local-Development-Setup:-o9Platform-Microservices-in-WSL.md
#
# Differences from the wiki version, all deliberate:
#   - /mnt/d/... -> macOS paths
#   - `docker run -d` so start-ls does not block the terminal
#   - --platform linux/amd64 (the ACR LS image is an amd64 build)
#   - image overridable via $O9_LS_IMAGE, for running your own build
#   - bash -> zsh-safe
#   - win-health / fix-time dropped: no separate Windows host, and macOS syncs
#     its own clock
#
# The `-e` environment variables are unchanged from the wiki: they already use
# host.docker.internal, which is *more* correct here than on WSL, where the
# Windows host was a genuinely separate machine.
{ pkgs, ... }:
let
  repoRoot  = "/Users/tikhon.jelvis/Programming/work/ls";
  common    = "${repoRoot}/o9.MicroService.Common";
  overrides = "${common}/macos-overrides";
  dataRoot  = "/Users/tikhon.jelvis/o9/data";

  # NOTE the doubled path segment: o9.Framework nests its content one level
  # down, which the wiki's paths omit.
  itCompose = "${repoRoot}/o9.Framework/o9.Framework/o9.Framework.IntegrationTests/MockWebApi/docker-compose.yaml";
in
{
  programs.zsh = {
    shellAliases = {
      start-infra = "docker compose -f ${common}/startup/infra.yaml -f ${overrides}/infra.macos.yaml up -d";
      stop-infra  = "docker compose -f ${common}/startup/infra.yaml down";

      start-sql-server = "docker compose -f ${common}/compose-files/sql-server.yaml -f ${overrides}/sql-server.macos.yaml up -d";
      stop-sql-server  = "docker compose -f ${common}/compose-files/sql-server.yaml down";

      # 5 services -- prefer this over the full 52-service set
      start-api-reqd = "docker compose -f ${common}/startup/web-api-required.yaml up -d";
      stop-api-reqd  = "docker compose -f ${common}/startup/web-api-required.yaml down";

      start-all-microservices      = "docker compose -f ${common}/no-webapi/docker-compose.yaml up -d";
      start-all-microservices-pull = "docker compose -f ${common}/no-webapi/docker-compose.yaml up -d --pull always";
      stop-all-microservices       = "docker compose -f ${common}/no-webapi/docker-compose.yaml down";

      start-kibo = "docker compose -f ${common}/startup/kibo.yaml up -d";
      stop-kibo  = "docker compose -f ${common}/startup/kibo.yaml down";

      start-ssl = "docker compose -f ${common}/ssl-proxy/docker-compose.yaml up -d";
      stop-ssl  = "docker compose -f ${common}/ssl-proxy/docker-compose.yaml down";

      # o9.Framework integration tests: no SQL Server, no ACR, all arm64-native
      start-test-infra = "docker compose -f ${itCompose} up -d --build";
      stop-test-infra  = "docker compose -f ${itCompose} down";
    };

    initContent = ''
      # ---- o9 Live Server helpers ----

      # start-ls's update_ls_instance issues an UPDATE, not an INSERT, so it
      # silently does nothing if no row exists for the tenant. Use this to check.
      # -W trims trailing spaces, which is what makes this readable: without it the
      # nvarchar columns pad to their declared width. (-y also caps width but is
      # mutually exclusive with -W, so use one or the other, not both.)
      ls-instances() {
        docker exec sqlserver /opt/mssql-tools18/bin/sqlcmd \
          -S localhost -U sa -P 'dbAdmin123##' -d o9.TenantModel -N -C -W -s" | " \
          -Q "SELECT ServiceName, Host, Port, DataFolder FROM LiveServerInstances ORDER BY ServiceName"
      }

      update_ls_instance() {
        local tenantname="$1"
        [[ -z "$tenantname" ]] && { echo "Error: Tenant name required"; return 1; }

        docker ps --filter "name=sqlserver" --format '{{.Names}}' | grep -q "^sqlserver$" || {
          echo "Error: SQL Server container not running"; return 1;
        }

        # The container can be "running" while sqlservr itself is dead: restore.sh
        # ends with `tail -f /dev/null`, so an OOM-killed SQL Server leaves the
        # container up (and Docker's restart policy never fires). Probe for real,
        # otherwise a failed query is indistinguishable from "0 rows" below.
        if ! docker exec sqlserver /opt/mssql-tools18/bin/sqlcmd \
               -S localhost -U sa -P 'dbAdmin123##' -d o9.TenantModel -N -C \
               -Q "SELECT 1" >/dev/null 2>&1; then
          echo "Error: SQL Server is not accepting connections (container is up but sqlservr may be dead)."
          echo "       Check: docker inspect -f '{{.State.OOMKilled}}' sqlserver"
          echo "       Fix:   docker restart sqlserver"
          return 1
        fi

        local count
        count=$(docker exec sqlserver /opt/mssql-tools18/bin/sqlcmd \
          -S localhost -U sa -P 'dbAdmin123##' -d o9.TenantModel -N -C \
          -Q "SET NOCOUNT ON; SELECT COUNT(*) FROM LiveServerInstances WHERE ServiceName = 'o9.LS-$tenantname' AND Host = ServiceName" \
          -h -1 -W | tr -cd '[:digit:]')

        if [[ "$count" -gt 0 ]]; then
          echo "LiveServerInstance already correct for $tenantname"
          return 0
        fi

        # Verify a row exists at all before claiming success
        local exists
        exists=$(docker exec sqlserver /opt/mssql-tools18/bin/sqlcmd \
          -S localhost -U sa -P 'dbAdmin123##' -d o9.TenantModel -N -C \
          -Q "SET NOCOUNT ON; SELECT COUNT(*) FROM LiveServerInstances WHERE ServiceName = 'o9.LS-$tenantname'" \
          -h -1 -W | tr -cd '[:digit:]')

        if [[ "''${exists:-0}" -eq 0 ]]; then
          echo "WARNING: no LiveServerInstances row for 'o9.LS-$tenantname'."
          echo "         update_ls_instance only UPDATEs, so there is nothing to change."
          echo "         Run ls-instances to see what exists."
          return 1
        fi

        docker exec sqlserver /opt/mssql-tools18/bin/sqlcmd \
          -S localhost -U sa -P 'dbAdmin123##' -d o9.TenantModel -N -C \
          -Q "UPDATE LiveServerInstances SET Host = ServiceName, Port = '27884', DataFolder = '/data', LocalTempPath = '/tmp' \
              WHERE ServiceName = 'o9.LS-$tenantname'" \
          >/dev/null 2>&1 && echo "Updated LiveServerInstance for $tenantname" \
                          || { echo "Update LiveServerInstance failed"; return 1; }
      }

      start-ls() {
        local tenantname="''${1:-unittest}"
        local network="''${2:-o9platform-network}"
        local data_path="$3"
        local image="''${O9_LS_IMAGE:-o9platform.azurecr.io/master/o9.graphcube.grpcservice:latest}"

        stop-ls "$tenantname" >/dev/null 2>&1

        local port=27111 max_port=27999 found_port=false
        while (( port <= max_port )); do
          if nc -z -w 1 localhost $port >/dev/null 2>&1; then
            (( port++ ))
          else
            found_port=true; break
          fi
        done
        if [[ "$found_port" != "true" ]]; then
          echo "Error: no unused port in 27111-$max_port."; return 1
        fi

        if [[ -z "$data_path" ]]; then
          if [[ -d "${dataRoot}/$tenantname" ]]; then
            data_path="${dataRoot}/$tenantname"
          else
            echo "Error: no data folder at ${dataRoot}/$tenantname"
            echo "       Pass one explicitly: start-ls $tenantname \"\" /path/to/data"
            return 1
          fi
        fi

        echo "  tenant: $tenantname   port: $port"
        echo "  data:   $data_path"
        echo "  image:  $image"

        update_ls_instance "$tenantname" || return 1

        docker run -d \
          --name "o9.LS-''${tenantname}" \
          --platform linux/amd64 \
          -p "''${port}:27884" \
          -v "''${data_path}:/data" \
          --network "$network" \
          --add-host host.docker.internal:host-gateway \
          --entrypoint bash \
          -e "o9LogDb=mongodb://host.docker.internal/?w=0" \
          -e "BaseUrl=http://router" \
          -e "LiveCacheConnectionString=Data Source=host.docker.internal,6379;database=0;syncTimeout=1000" \
          -e "MQPort=5672" \
          -e "KiboHost=http://host.docker.internal:14223/kibo2" \
          -e "MQHost=host.docker.internal" \
          -e "HARabbitMQUserName=guest" \
          -e "HARabbitMQPassword=guest" \
          -e o9Config="Data Source=host.docker.internal,1437;Initial Catalog=o9.TenantModel;Integrated Security=false;User ID=sa;Password=dbAdmin123##;Encrypt=true;MultipleActiveResultSets=True;TrustServerCertificate=true" \
          "$image" \
          -c "dotnet o9.GraphCube.GrpcService.dll -TenantName:''${tenantname}" \
          && echo "Started o9.LS-''${tenantname} on port $port" \
          && echo "  logs:   docker logs -f o9.LS-''${tenantname}"
      }

      stop-ls() {
        local tenantname="''${1:-unittest}"
        local container_name="o9.LS-''${tenantname}"

        if ! docker ps -a --format '{{.Names}}' | grep -q "^''${container_name}$"; then
          echo "Container '$container_name' does not exist."; return 1
        fi
        if docker ps --format '{{.Names}}' | grep -q "^''${container_name}$"; then
          docker stop "$container_name" >/dev/null && echo "Stopped $container_name"
        fi
        docker rm "$container_name" >/dev/null && echo "Removed $container_name"
      }
    '';
  };
}
