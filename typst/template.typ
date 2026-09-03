#let render-inline(part) = {if "Text" in part {
    part.Text
  } else if "Code" in part {
    raw(part.Code)
  } else if "Link" in part {
    link(part.Link.url)[#part.Link.text]
  } else {
    panic("unknown inline variant")
  }
}

#let render-separated(items, separator: [ · ]) = {
  for (index, item) in items.enumerate() {
    if index > 0 {
      separator
    }

    item
  }
}

#let render-inlines(parts) = {
  for part in parts {
    render-inline(part)
  }
}

#let contact-icons = align(center)[
  #box(image("../icons/nixos.svg", height: 0.9em))
  #box(image("../icons/terraform.svg", height: 0.9em))
  #box(image("../icons/docker.svg", height: 0.9em))
  #box(image("../icons/matrix.svg", height: 0.9em))
  #box(image("../icons/rust.svg", height: 0.9em))
  #box(image("../icons/python.svg", height: 0.9em))
  #box(image("../icons/typescript.svg", height: 0.9em))
  #box(image("../icons/haskell.svg", height: 0.9em))
]

#let skill-strip(skills) = {
  if skills.len() > 0 {
    v(0.45em)
    align(center)[
      #text(size: 7.5pt, fill: rgb("#555555"))[#render-separated(skills)]
    ]
    v(0.55em)
  }
}

#let impact-tags(impact) = {
  if impact.len() > 0 {
    v(0.12em)
    text(size: 7.2pt, fill: rgb("#555555"))[#render-separated(impact)]
    v(0.08em)
  }
}

#let job-entry(job) = {
  v(0.75em)

  grid(
    columns: (1fr, auto),
    align: (left, right),
    strong(job.organization),
    text(size: 8.8pt)[#emph(job.position + " (" + job.duration + ")") ],
  )

  impact-tags(job.impact)
  v(0.2em)

  list(
    ..job.experiences.map(exp => [#render-inlines(exp)]),
  )
}

#let resume-document(resume: (:), body) = {
  set page(paper: "us-letter", margin: (x: 0.7in, y: 0.6in))
  set text(font: ("Helvetica", "Liberation Sans", "DejaVu Sans"), size: 9.8pt)
  set par(leading: 0.48em, spacing: 0em)
  set list(marker: [•], indent: 1.15em, body-indent: 0.45em, spacing: 0.16em)

  text(size: 18pt, weight: "bold")[#resume.contact.name]
  v(0.25em)

  line(length: 100%, stroke: 0.45pt)
  v(0.55em)

  grid(
    columns: (1fr, auto, 1fr),
    align: (left, center, right),
    link(resume.contact.github)[#resume.contact.github],
    contact-icons,
    link("mailto:" + resume.contact.email)[#resume.contact.email],
  )

  skill-strip(resume.skills)

  v(1em)
  text(size: 11pt, weight: "bold")[Work Experience]
  v(0.35em)
  line(length: 100%, stroke: 0.35pt)
  v(0.25em)

  for job in resume.history {
    job-entry(job)
  }

  v(0.45em)
  line(length: 100%, stroke: 0.35pt)

  body
}

#resume-document(resume: json("../build/resume.json"), "")
