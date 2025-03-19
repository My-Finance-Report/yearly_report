from fastapi import APIRouter
from fastapi.responses import Response


router = APIRouter(prefix="", tags=["sitemap"])

@router.get("/sitemap.xml")
def sitemap():
    pages = [
        "/",
        "/faq",
        "/privacy",
        "/demo",
        "/landing"
        "/how",
        "/contact-me",
    ]
    xml_content = f"""<?xml version="1.0" encoding="UTF-8"?>
    <urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
        {''.join([f"<url><loc>https://myfinancereport.com{page}</loc><priority>0.8</priority></url>" for page in pages])}
    </urlset>"""
    return Response(content=xml_content, media_type="application/xml")
