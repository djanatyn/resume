#let render-inline(part) = {
  if "Text" in part {
    part.Text
  } else if "Code" in part {
    raw(part.Code)
  } else if "Link" in part {
    link(part.Link.url)[#part.Link.text]
  } else {
    panic("unknown inline variant")
  }
}

#let render-inlines(parts) = {
  for part in parts {
    render-inline(part)
  }
}

#let job-entry(job) = {
  grid(
    columns: (1fr, auto),
    align: (left, right),
    strong(job.organization),
    emph(job.position + " (" + job.duration + ")"),
  )

  list(
    tight: true,
    ..job.experiences.map(exp => [#render-inlines(exp)]),
  )
}

#let resume-document(resume: (:), body) = {
  set page(paper: "us-letter", margin: (x: 0.65in, y: 0.55in))
  set text(font: "Arial", size: 10pt)
  set par(leading: 0.4em, spacing: 0.45em)

  [
    #text(size: 20pt)[#resume.contact.name]
  ]

  line(length: 100%, stroke: 0.6pt)

  grid(
    columns: (1fr, auto, 1fr),
    align: (left, center, right),
    link(resume.contact.github)[#resume.contact.github],
    align(center)[
      #box(image("../icons/haskell.svg", height: 1.1em))
      #box(image("../icons/nixos.svg", height: 1.1em))
      #box(image("../icons/rust.svg", height: 1.1em))
      #box(image("../icons/typescript.svg", height: 1.1em))
    ],
    link("mailto:" + resume.contact.email)[#resume.contact.email],
  )

  heading(level: 1)[Work Experience]

  for job in resume.history {
    job-entry(job)
  }

  line(length: 100%, stroke: 0.6pt)

  body
}

#resume-document(resume: json("../build/resume.json"), "")
