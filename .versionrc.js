const yaml = require("yaml");

module.exports = {
  packageFiles: [
    {
      filename: "DESCRIPTION",
      updater: {
        readVersion: (contents) => {
          const v = yaml.parse(contents, "utf8")
          return v.Version
        },
        writeVersion: (contents, version) => {
          const yamlFile = yaml.parse(contents, "utf8")
          yamlFile.version = version
          return yaml.stringify(yamlFile, "utf8")
        }
      }
    }
  ]
}
