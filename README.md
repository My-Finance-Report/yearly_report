# Big picture

this started as a project to generate my yearly financial report using haskell
and has evolved a bit to be a [simple webapp](https://myfinancereport.com/).

# Features

- parse PDF bank statements and credit card statements
- categorize expenses
- generate reports
  - per month drill down
  - summary -> per transaction drill down
  - configurable sankey and column diagrams
- allow for manually update transactions from a ui

# Tech

- haskell for backend and webserver
- html for report
- postgres to persist data
- LLM for PDF parsing (using structured outputs)
- LLM for categorization (using structured outputs)

# Disclaimer

this is mainly a project through which I will learn more about haskell, but I expect the code to be bad.

# User feedback

- the site seems sluggish
- sankey generation will have to come from somewhere now that no onboarding
- make sure we have jobs for all files after migration
- probably on the act of actually uploading (not a rerun)
  => it seems like there is a way to upload a file twice
  -> i actually dont even think there is a constraint that prevents this

- auto add categories and accounts ?
- maybe reprocess when you update a category
- (still) need to preserve location in tab groups when navigating forms
- enable config of the barchart (categories to include + sources to include) (wip)

- uploads fail if the transactions source does not have any categories
  => constrain the db to prevent this
- shareable report
  - allow to select different charts
- dont show snakey if there is a cycle in the chart, and alert?
- release an admin page
- why is it so slow when i upload n files in the first pass?
  - probably some async code we dont need anymore
- we should save some error logs to the db when a job fails
- update the demo data to be more anon
- manually add transactions
- budget and budget progress
- option to add a new source on the upload page after you drop in the file
- privacy policy
- install a linter for extraneous imports
- memoize llm calls to preserve existing data save costs

DROP TABLE IF EXISTS processed_file;

ALTER TABLE process_file_job
DROP CONSTRAINT process_file_job_pdf_id_fkey,
ADD CONSTRAINT process_file_job_pdf_id_fkey
FOREIGN KEY (pdf_id)
REFERENCES uploaded_pdf (id)
ON DELETE CASCADE;

SELECT raw*content_hash, user_id, COUNT(*)
FROM uploaded*pdf
GROUP BY raw_content_hash, user_id
HAVING COUNT(*) > 1;

WITH ranked AS (
SELECT
id,
MIN(id) OVER (PARTITION BY raw_content_hash, user_id) AS master_id
FROM uploaded_pdf
)
UPDATE transaction
SET uploaded_pdf_id = ranked.master_id
FROM ranked
WHERE transaction.uploaded_pdf_id = ranked.id
AND ranked.id <> ranked.master_id;

WITH ranked AS (
SELECT
id,
ROW_NUMBER() OVER (
PARTITION BY raw_content_hash, user_id
ORDER BY id ASC
) AS row_num
FROM uploaded_pdf
)
DELETE FROM uploaded_pdf
WHERE id IN (
SELECT id
FROM ranked
WHERE row_num > 1
);

INSERT INTO process_file_job (
createdAt,
lastTriedAt,
status,
userId,
configId,
pdfId,
archived,
attemptCount
)
SELECT
NOW() AS createdAt,
NULL AS lastTriedAt,
'Completed' AS status, -- or 'Completed', depending on your enum
up.userId,
123 AS configId, -- pick a real config or default
up.id AS pdfId,
FALSE AS archived,
0 AS attemptCount
FROM uploaded_pdf up
LEFT JOIN process_file_job pfj
ON pfj.pdfId = up.id
AND pfj.userId = up.userId
WHERE pfj.id IS NULL
;
