import Foundation

struct FileCache {
    static let root = FileManager.default
        .urls(for: .cachesDirectory, in: .userDomainMask)[0]
        .appending(component: "YachtVAA")

    static let currentsDir = root.appending(component: "currents")

    /// Returns the URL for a SISCORAR GeoJSON file by ISO date string (e.g. "2025-02-25").
    static func currentGridURL(date: String) -> URL {
        currentsDir.appending(component: "\(date).geojson")
    }

    /// Creates the currents directory if needed.
    static func prepare() throws {
        try FileManager.default.createDirectory(
            at: currentsDir,
            withIntermediateDirectories: true
        )
    }

    /// Removes all cached GeoJSON files.
    static func clearCurrentGrids() throws {
        let fm = FileManager.default
        guard fm.fileExists(atPath: currentsDir.path) else { return }
        let files = try fm.contentsOfDirectory(
            at: currentsDir,
            includingPropertiesForKeys: nil
        )
        for url in files where url.pathExtension == "geojson" {
            try fm.removeItem(at: url)
        }
    }

    /// Approximate size of the currents cache in bytes.
    static func currentGridsSize() -> Int {
        let fm = FileManager.default
        guard fm.fileExists(atPath: currentsDir.path),
              let files = try? fm.contentsOfDirectory(
                  at: currentsDir,
                  includingPropertiesForKeys: [.fileSizeKey]
              ) else { return 0 }
        return files.reduce(0) { total, url in
            let size = (try? url.resourceValues(forKeys: [.fileSizeKey]).fileSize) ?? 0
            return total + size
        }
    }
}
