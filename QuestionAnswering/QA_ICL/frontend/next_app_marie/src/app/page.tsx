import { promises as fs } from 'fs';
import path from "path";

import * as React from "react";
import QAFragment from './_components/qa';
import IntroSection from "../components/intro-section";
import { ExampleQuestionGroup } from './_components/example-question-accordion';

export default async function Home() {
  const introText = await fs.readFile(path.join(process.cwd(), "resources/intro-text.md"), "utf8")
  const exampleQuestionGroups: ExampleQuestionGroup[] = JSON.parse(await fs.readFile(path.join(process.cwd(), "resources/example-questions.json"), "utf-8"))

  return (
    <main className="p-4 min-h-screen flex flex-col justify-center items-center">
      <IntroSection imgSrc='/images/marie-thumbnail.jpg' imgAlt="Marie's thumbnail" introTextMd={introText} />
      <QAFragment exampleQuestionGroups={exampleQuestionGroups} />
    </main>
  );
}
