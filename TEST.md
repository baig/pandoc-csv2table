# Tests

The different table formats supported by `pandoc-csv2table` are exercised in
`Examples/example.md`. The file `examples-postprocessed.markdown` contains the
output generated by previous versions of `pandoc-csv2table`. After every change
to pandoc-csv2table, ensure that there are no differences in the contents of
`examples-postprocessed.markdown`.

~~~
$ stack install --resolver nightly
$ cd Examples
$ stack exec --resolver nightly -- pandoc --filter=pandoc-csv2table example.md \
  -t markdown > examples-postprocessed.markdown
$ git diff -- examples-postprocessed.markdown
~~~
