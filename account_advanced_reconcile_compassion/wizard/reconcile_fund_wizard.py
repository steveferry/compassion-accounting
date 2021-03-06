﻿# -*- encoding: utf-8 -*-
##############################################################################
#
#    Copyright (C) 2014 Compassion CH (http://www.compassion.ch)
#    Releasing children from poverty in Jesus' name
#    @author: Cyril Sester <csester@compassion.ch>
#
#    The licence is in the file __openerp__.py
#
##############################################################################

import time
from openerp.osv import orm, fields
from openerp.tools.translate import _
from openerp import netsvc


class reconcile_fund_wizard(orm.TransientModel):
    """Wizard that helps the user doing a full reconciliation when a customer
    paid more than excepted. It puts the extra amount in a fund selected
    in the wizard and fully reconcile the credit line. """
    _name = 'reconcile.fund.wizard'

    def _get_default_ids(self, cr, uid, context=None):
        # The ids of the move_lines are given in the context, so
        # we don't use the 'ids' fields and put [0] in it.
        return self._get_contract_ids(cr, uid, [0], 'contract_id', '',
                                      context)[0]

    def _get_contract_ids(self, cr, uid, ids, field_name, arg, context):
        move_line_obj = self.pool.get('account.move.line')
        contract_ids = set()
        active_ids = context.get('active_ids')
        if active_ids:
            for move_line in move_line_obj.browse(cr, uid, active_ids,
                                                  context):
                if move_line and move_line.debit > 0:
                    invoice = move_line.invoice
                    if invoice and invoice.amount_total == move_line.debit:
                        contract_ids.update([invoice_line.contract_id.id
                                             for invoice_line in
                                             invoice.invoice_line
                                             if invoice_line.contract_id])
        return {id: list(contract_ids) for id in ids}

    def _write_contracts(self, cr, uid, ids, field_name, field_value, arg,
                         context):
        value_obj = self.pool.get('recurring.contract')
        for line in field_value:
            if line[0] == 1:  # one2many update
                value_id = line[1]
                value_obj.write(cr, uid, [value_id], line[2])
        return True

    _columns = {
        'fund_id': fields.many2one('product.product', 'Fund', required=True),
        'contract_ids': fields.function(
            _get_contract_ids, fnct_inv=_write_contracts, type='one2many',
            obj='recurring.contract', method=True,
            string=_('Related contracts'),
            help=_('You can directly edit the contracts from here if you want '
                   'to add a line for the fund.')),
    }

    def _get_general_fund(self, cr, uid, context=None):
        general_fund = self.pool.get('product.product').search(
            cr, uid, [('name', '=', 'General Fund')], context=context)
        return general_fund and general_fund[0] or False

    _defaults = {
        'fund_id': _get_general_fund,
        'contract_ids': _get_default_ids,
    }

    def reconcile_with_fund(self, cr, uid, ids, context=None):
        ''' Generate an invoice corresponding to the selected fund
            and reconcile it with selected move lines
        '''
        if isinstance(ids, list):
            ids = ids[0]

        wizard = self.browse(cr, uid, ids, context)
        contract_ids = [c.id for c in wizard.contract_ids]
        if not contract_ids:
            raise orm.except_orm(
                _('No contract'),
                _('This operation is only allowed for invoices related to '
                  'sponsorships.'))
        active_ids = context.get('active_ids')
        res = {}
        invoice_obj = self.pool.get('account.invoice')
        move_line_obj = self.pool.get('account.move.line')
        invoice_found = False
        residual = 0.0

        for line in move_line_obj.browse(cr, uid, active_ids,
                                         context):
            residual += line.credit - line.debit
            if not invoice_found and line.debit > 0:
                invoice_id = line.invoice.id
                account_id = line.invoice.account_id.id
                partner_id = line.partner_id.id
                invoice_found = True
                active_ids.remove(line.id)

        if residual <= 0:
            raise orm.except_orm(
                'ResidualError',
                _('This can only be done if credits > debits'))

        if invoice_id:
            invoice_obj.action_cancel(cr, uid, [invoice_id], context)
            invoice_obj.action_cancel_draft(cr, uid, [invoice_id], context)
            invoice = invoice_obj.browse(cr, uid, invoice_id, context)

            res.update(self._generate_invoice_line(
                cr, uid, invoice_id, wizard.fund_id,
                residual, partner_id, contract_ids, context=context))

            # Validate the invoice
            wf_service = netsvc.LocalService('workflow')
            wf_service.trg_validate(
                uid, 'account.invoice', invoice_id, 'invoice_open', cr)
            move_line_ids = move_line_obj.search(
                cr, uid, [('move_id', '=', invoice.move_id.id),
                          ('account_id', '=', account_id)],
                context=context)
            active_ids.extend(move_line_ids)
            move_line_obj.reconcile(cr, uid, active_ids, 'manual',
                                    context=context)

        return {'type': 'ir.actions.act_window_close'}

    def _generate_invoice_line(self, cr, uid, invoice_id, product, price,
                               partner_id, contract_ids, context=None):

        inv_line_data = {
            'name': product.name,
            'account_id': product.property_account_income.id,
            'price_unit': price / len(contract_ids),
            'quantity': 1,
            'uos_id': False,
            'product_id': product.id or False,
            'invoice_id': invoice_id,
        }

        res = {}

        # Define analytic journal
        analytic = self.pool.get('account.analytic.default').account_get(
            cr, uid, product.id, partner_id, uid, time.strftime('%Y-%m-%d'),
            context=context)
        if analytic and analytic.analytics_id:
            inv_line_data['analytics_id'] = analytic.analytics_id.id

        res['name'] = product.name

        for contract_id in contract_ids:
            inv_line_data['contract_id'] = contract_id
            self.pool.get('account.invoice.line').create(
                cr, uid, inv_line_data, context=context)
        return res
