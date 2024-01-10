FROM dataeditors/stata18:2023-12-20

COPY Setup.do Setup.do
RUN --mount=type=secret,id=statalic,uid=1000,dst=/usr/local/stata/stata.lic stata-mp do /project/Setup.do

ENTRYPOINT ["stata-mp","/project/Replication code.do"]
