var path = require('path');

// Main

exports.root = path.join(path.dirname(process.argv[1]), '..');

exports.elmPackage = path.join(exports.root, 'elm-package.json');
var elmPackageContents = require(exports.elmPackage);

exports.sourceSubdirectory = elmPackageContents['source-directories'][0]
exports.sourceDirectory = path.join(exports.root, exports.sourceSubdirectory);
exports.sourceRootModule = path.join(exports.sourceDirectory, elmPackageContents['exposed-modules'][0]);
exports.mainFilename = 'Main.elm';
exports.source = path.join(exports.sourceRootModule, exports.mainFilename);

exports.targetDirectory = path.join(exports.root, 'target');
exports.target = path.join(exports.targetDirectory, 'elm.js')

// Test

exports.testDirectory = path.join(exports.root, 'test');

exports.testElmPackage = path.join(exports.testDirectory, 'elm-package.json');
var testElmPackageContents = require(exports.testElmPackage);

exports.testSourceSubdirectory = testElmPackageContents['source-directories'][0]
exports.testSourceDirectory = path.join(exports.testDirectory, exports.testSourceSubdirectory);
exports.testSourceRootModule = path.join(exports.testSourceDirectory, elmPackageContents['exposed-modules'][0]);
exports.testFilename = 'Test.elm';
exports.testSource = path.join(exports.testSourceRootModule, exports.testFilename);

exports.testTargetDirectory = path.join(exports.testDirectory, 'target');
exports.testTarget = path.join(exports.testTargetDirectory, 'index.html');
