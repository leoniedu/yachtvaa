import SwiftUI

struct ConditionsView: View {
    @EnvironmentObject private var session: AppSession

    private let headers = ["Hora", "Vento", "Dir.", "Corrente SISCORAR", "Dir.", "Boia", "Dir."]

    var body: some View {
        if session.buoyReadings.isEmpty && session.currentGrid == nil {
            PlaceholderView(
                icon: "wind",
                title: "Sem dados de condições",
                subtitle: "Toque Carregar para buscar dados ambientais do dia."
            )
        } else {
            conditionsTable
        }
    }

    private var conditionsTable: some View {
        let byHour = hourlyBuoy()
        // SISCORAR is keyed by UTC hour; convert to local Bahia hours for display alignment.
        let utcOffsetHours = AppConfig.timezone.secondsFromGMT() / 3600  // -3 for Bahia
        let siscorar: [Int: (speedMps: Double, dirDeg: Double)]
        if let grid = session.currentGrid {
            let utc = EnvironmentalMatcher.hourlyCurrentAtPoint(
                lat: AppConfig.buoyLat, lon: AppConfig.buoyLon, grid: grid
            )
            siscorar = utc.reduce(into: [:]) { result, pair in
                let localHour = (pair.key + utcOffsetHours + 24) % 24
                result[localHour] = pair.value
            }
        } else {
            siscorar = [:]
        }
        return ScrollView([.vertical, .horizontal]) {
            VStack(alignment: .leading, spacing: 0) {
                headerRow
                Divider()
                ForEach(sessionHours(), id: \.self) { hour in
                    dataRow(hour: hour, buoy: byHour[hour], siscorar: siscorar[hour])
                    Divider()
                }
            }
        }
    }

    /// Bahia local hours covered by the GPS session; falls back to 0-23 if no session loaded.
    /// h0 = floor(gpsStart hour), h1 = ceil(gpsEnd hour) so on-the-hour buoy/SISCORAR
    /// readings at the edges are always included.
    private func sessionHours() -> [Int] {
        guard let start = session.gpsStart, let end = session.gpsEnd else {
            return Array(0..<24)
        }
        var cal = Calendar(identifier: .gregorian)
        cal.timeZone = AppConfig.timezone
        let h0 = cal.component(.hour, from: start)
        let endMins = cal.component(.minute, from: end)
        let h1 = min(cal.component(.hour, from: end) + (endMins > 0 ? 1 : 0), 23)
        return h0 <= h1 ? Array(h0...h1) : Array(0..<24)
    }

    private func hourlyBuoy() -> [Int: BuoyReading] {
        var cal = Calendar(identifier: .gregorian)
        cal.timeZone = AppConfig.timezone
        var result: [Int: BuoyReading] = [:]
        for r in session.buoyReadings {
            let h = cal.component(.hour, from: r.ts)
            result[h] = r
        }
        return result
    }

    private var headerRow: some View {
        HStack(spacing: 0) {
            ForEach(Array(headers.enumerated()), id: \.offset) { _, h in
                Text(h)
                    .font(.caption.bold())
                    .frame(width: colWidth(h), alignment: .leading)
                    .padding(.horizontal, 8)
                    .padding(.vertical, 6)
            }
        }
        .background(.regularMaterial)
    }

    private func dataRow(hour: Int, buoy: BuoyReading?, siscorar: (speedMps: Double, dirDeg: Double)?) -> some View {
        HStack(spacing: 0) {
            cell(String(format: "%02d:00", hour), width: colWidth(headers[0]), hasData: true)
            cell(fmt(buoy?.windSpeedMps, "%.1f"), width: colWidth(headers[1]), hasData: buoy?.windSpeedMps != nil)
            cell(fmtDeg(buoy?.windDirectionDeg), width: colWidth(headers[2]), hasData: buoy?.windDirectionDeg != nil)
            cell(fmt(siscorar?.speedMps, "%.3f"), width: colWidth(headers[3]), hasData: siscorar != nil)
            cell(fmtDeg(siscorar?.dirDeg), width: colWidth(headers[4]), hasData: siscorar != nil)
            cell(fmt(buoy?.currentSpeedMps, "%.3f"), width: colWidth(headers[5]), hasData: buoy?.currentSpeedMps != nil)
            cell(fmtDeg(buoy?.currentDirectionDeg), width: colWidth(headers[6]), hasData: buoy?.currentDirectionDeg != nil)
        }
    }

    private func cell(_ text: String, width: CGFloat, hasData: Bool) -> some View {
        Text(text)
            .font(.caption.monospacedDigit())
            .foregroundStyle(hasData ? Color.primary : Color.secondary.opacity(0.4))
            .frame(width: width, alignment: .leading)
            .padding(.horizontal, 8)
            .padding(.vertical, 4)
    }

    private func fmt(_ val: Double?, _ format: String) -> String {
        guard let v = val else { return "—" }
        return String(format: format, v)
    }

    private func fmtDeg(_ val: Double?) -> String {
        guard let v = val else { return "—" }
        return String(format: "%.0f°", v)
    }

    private func colWidth(_ header: String) -> CGFloat {
        header == "Hora" ? 52 : header.contains("Corrente") ? 110 : 60
    }
}
