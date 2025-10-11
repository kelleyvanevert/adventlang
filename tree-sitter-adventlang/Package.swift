// swift-tools-version:5.3

import Foundation
import PackageDescription

var sources = ["src/parser.c"]
if FileManager.default.fileExists(atPath: "src/scanner.c") {
    sources.append("src/scanner.c")
}

let package = Package(
    name: "TreeSitterAdventlang",
    products: [
        .library(name: "TreeSitterAdventlang", targets: ["TreeSitterAdventlang"]),
    ],
    dependencies: [
        .package(url: "https://github.com/tree-sitter/swift-tree-sitter", from: "0.8.0"),
    ],
    targets: [
        .target(
            name: "TreeSitterAdventlang",
            dependencies: [],
            path: ".",
            sources: sources,
            resources: [
                .copy("queries")
            ],
            publicHeadersPath: "bindings/swift",
            cSettings: [.headerSearchPath("src")]
        ),
        .testTarget(
            name: "TreeSitterAdventlangTests",
            dependencies: [
                "SwiftTreeSitter",
                "TreeSitterAdventlang",
            ],
            path: "bindings/swift/TreeSitterAdventlangTests"
        )
    ],
    cLanguageStandard: .c11
)
