// visionocr <image.png> ... — emit TSV: x, y, w, text for each Vision observation
import Foundation
import Vision
import AppKit

guard CommandLine.arguments.count >= 2 else {
    FileHandle.standardError.write("usage: visionocr <image> [image ...]\n".data(using: .utf8)!)
    exit(1)
}

for path in CommandLine.arguments.dropFirst() {
    guard let img = NSImage(contentsOfFile: path),
          let cg = img.cgImage(forProposedRect: nil, context: nil, hints: nil) else {
        FileHandle.standardError.write("cannot read \(path)\n".data(using: .utf8)!)
        continue
    }
    let request = VNRecognizeTextRequest()
    request.recognitionLevel = .accurate
    request.usesLanguageCorrection = false
    request.recognitionLanguages = ["en-US"]
    let handler = VNImageRequestHandler(cgImage: cg, options: [:])
    try? handler.perform([request])
    print("===FILE:\(path)===")
    guard let obs = request.results else { continue }
    for o in obs {
        guard let c = o.topCandidates(1).first else { continue }
        let b = o.boundingBox
        let t = c.string.replacingOccurrences(of: "\t", with: " ")
        print("\(b.minX)\t\(1.0 - b.maxY)\t\(b.width)\t\(b.height)\t\(t)")
    }
}
