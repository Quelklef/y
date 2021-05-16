exports.encodeUtf8 = string => {
  return Buffer.from(string, "utf-8");
};

exports.decodeUtf8 = buffer => {
  return buffer.toString("utf-8");
};
