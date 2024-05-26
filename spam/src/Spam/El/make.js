const { readFileSync, writeFileSync } = require("fs");

const head = `module Spam.El.Html where
import Spam.El (El(Tag))
`;

const tags = JSON.parse(readFileSync("./elements.json"));
const document = head + tags.map((x) => `${x}' = Tag "${x}"`).join("\n");

writeFileSync("./Html.hs", document);
